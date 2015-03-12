/* -*- Mode: C; c-file-style: "gnu"; tab-width: 8 -*- */
/* gtkpathbar.c
 * Copyright (C) 2004  Red Hat, Inc.,  Jonathan Blandford <jrb@gnome.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library. If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"

#include "gtkpathbar.h"

#include <string.h>

#include "gtkbox.h"
#include "gtkdnd.h"
#include "gtkicontheme.h"
#include "gtkimage.h"
#include "gtkintl.h"
#include "gtklabel.h"
#include "gtkmain.h"
#include "gtkmarshalers.h"
#include "gtksettings.h"
#include "gtktogglebutton.h"
#include "gtkwidgetpath.h"
#include "gtkwidgetprivate.h"

struct _GtkPathBarPrivate
{
  GCancellable *get_info_cancellable;

  GdkWindow *event_window;

  GFile *current_path;

  GList *button_list;
  GList *first_scrolled_button;
  GtkWidget *up_slider_button;
  GtkWidget *down_slider_button;
  guint settings_signal_id;
  gint icon_size;
  gint16 slider_width;
  gint16 button_offset;
  guint timer;
  guint slider_visible : 1;
  guint need_timer     : 1;
  guint ignore_click   : 1;
  guint scrolling_up   : 1;
  guint scrolling_down : 1;
};

enum {
  PATH_CLICKED,
  LAST_SIGNAL
};

typedef enum {
  NORMAL_BUTTON,
  ROOT_BUTTON,
  HOME_BUTTON,
  MOUNT_BUTTON,
  DESKTOP_BUTTON
} ButtonType;

#define BUTTON_DATA(x) ((ButtonData *)(x))

#define INITIAL_SCROLL_TIMEOUT 300
#define SCROLL_TIMEOUT         150

#define BUTTON_BOTTOM_SHADOW 1

static guint path_bar_signals [LAST_SIGNAL] = { 0 };

/* Icon size for if we can't get it from the theme */
#define FALLBACK_ICON_SIZE 16

typedef struct _ButtonData ButtonData;

struct _ButtonData
{
  GtkWidget *button;
  ButtonType type;
  gboolean is_root;
  char *dir_name;
  GFile *file;
  GFileMonitor *monitor;
  guint file_changed_signal_id;
  GtkWidget *image;
  GtkWidget *label;
  GCancellable *cancellable;
  guint ignore_changes : 1;
  guint file_is_hidden : 1;
};

G_DEFINE_TYPE_WITH_PRIVATE (GtkPathBar, gtk_path_bar, GTK_TYPE_CONTAINER)

static void gtk_path_bar_finalize                 (GObject          *object);
static void gtk_path_bar_dispose                  (GObject          *object);
static void gtk_path_bar_realize                  (GtkWidget        *widget);
static void gtk_path_bar_unrealize                (GtkWidget        *widget);
static void gtk_path_bar_get_preferred_width      (GtkWidget        *widget,
                                                   gint             *minimum,
                                                   gint             *natural);
static void gtk_path_bar_get_preferred_height     (GtkWidget        *widget,
                                                   gint             *minimum,
                                                   gint             *natural);
static void gtk_path_bar_map                      (GtkWidget        *widget);
static void gtk_path_bar_unmap                    (GtkWidget        *widget);
static void gtk_path_bar_size_allocate            (GtkWidget        *widget,
						   GtkAllocation    *allocation);
static void gtk_path_bar_add                      (GtkContainer     *container,
						   GtkWidget        *widget);
static void gtk_path_bar_remove                   (GtkContainer     *container,
						   GtkWidget        *widget);
static void gtk_path_bar_forall                   (GtkContainer     *container,
						   gboolean          include_internals,
						   GtkCallback       callback,
						   gpointer          callback_data);
static GtkWidgetPath *gtk_path_bar_get_path_for_child (GtkContainer *container,
                                                       GtkWidget    *child);
static gboolean gtk_path_bar_scroll               (GtkWidget        *widget,
						   GdkEventScroll   *event);
static void gtk_path_bar_scroll_up                (GtkPathBar       *path_bar);
static void gtk_path_bar_scroll_down              (GtkPathBar       *path_bar);
static void gtk_path_bar_stop_scrolling           (GtkPathBar       *path_bar);
static gboolean gtk_path_bar_slider_button_press  (GtkWidget        *widget,
						   GdkEventButton   *event,
						   GtkPathBar       *path_bar);
static gboolean gtk_path_bar_slider_button_release(GtkWidget        *widget,
						   GdkEventButton   *event,
						   GtkPathBar       *path_bar);
static void gtk_path_bar_grab_notify              (GtkWidget        *widget,
						   gboolean          was_grabbed);
static void gtk_path_bar_state_changed            (GtkWidget        *widget,
						   GtkStateType      previous_state);
static void gtk_path_bar_style_updated            (GtkWidget        *widget);
static void gtk_path_bar_screen_changed           (GtkWidget        *widget,
						   GdkScreen        *previous_screen);
static void gtk_path_bar_check_icon_theme         (GtkPathBar       *path_bar);
static void gtk_path_bar_update_button_appearance_and_state (ButtonData       *button_data,
                                                             gboolean          current_dir);
static void gtk_path_bar_update_button_appearance (ButtonData       *button_data);
static void gtk_path_bar_on_file_changed (GFileMonitor      *monitor,
                                          GFile             *file,
                                          GFile             *new_file,
                                          GFileMonitorEvent  event_type,
                                          gpointer          *user_data);
static void gtk_path_bar_update_path_async (GTask *task);

static void
gtk_path_bar_init (GtkPathBar *path_bar)
{
  GtkStyleContext *context;

  path_bar->priv = gtk_path_bar_get_instance_private (path_bar);

  gtk_widget_init_template (GTK_WIDGET (path_bar));

  /* Add the children manually because GtkPathBar derives from an abstract class,
   * Glade cannot edit a <template> in gtkpathbar.ui if it's only a GtkContainer.
   */
  gtk_container_add (GTK_CONTAINER (path_bar), path_bar->priv->down_slider_button);
  gtk_container_add (GTK_CONTAINER (path_bar), path_bar->priv->up_slider_button);

  /* GtkBuilder wont let us connect 'swapped' without specifying the signal's
   * user data in the .ui file
   */
  g_signal_connect_swapped (path_bar->priv->up_slider_button, "clicked",
			    G_CALLBACK (gtk_path_bar_scroll_up), path_bar);
  g_signal_connect_swapped (path_bar->priv->down_slider_button, "clicked",
			    G_CALLBACK (gtk_path_bar_scroll_down), path_bar);

  gtk_widget_set_has_window (GTK_WIDGET (path_bar), FALSE);
  gtk_widget_set_redraw_on_allocate (GTK_WIDGET (path_bar), FALSE);

  context = gtk_widget_get_style_context (GTK_WIDGET (path_bar));
  gtk_style_context_add_class (context, "path-bar");
  gtk_style_context_add_class (context, GTK_STYLE_CLASS_LINKED);

  path_bar->priv->get_info_cancellable = NULL;
  path_bar->priv->icon_size = FALLBACK_ICON_SIZE;
}

static void
gtk_path_bar_class_init (GtkPathBarClass *path_bar_class)
{
  GObjectClass *gobject_class;
  GtkWidgetClass *widget_class;
  GtkContainerClass *container_class;

  gobject_class = (GObjectClass *) path_bar_class;
  widget_class = (GtkWidgetClass *) path_bar_class;
  container_class = (GtkContainerClass *) path_bar_class;

  gobject_class->finalize = gtk_path_bar_finalize;
  gobject_class->dispose = gtk_path_bar_dispose;

  widget_class->get_preferred_width = gtk_path_bar_get_preferred_width;
  widget_class->get_preferred_height = gtk_path_bar_get_preferred_height;
  widget_class->realize = gtk_path_bar_realize;
  widget_class->unrealize = gtk_path_bar_unrealize;
  widget_class->map = gtk_path_bar_map;
  widget_class->unmap = gtk_path_bar_unmap;
  widget_class->size_allocate = gtk_path_bar_size_allocate;
  widget_class->style_updated = gtk_path_bar_style_updated;
  widget_class->screen_changed = gtk_path_bar_screen_changed;
  widget_class->grab_notify = gtk_path_bar_grab_notify;
  widget_class->state_changed = gtk_path_bar_state_changed;
  widget_class->scroll_event = gtk_path_bar_scroll;

  container_class->add = gtk_path_bar_add;
  container_class->forall = gtk_path_bar_forall;
  container_class->remove = gtk_path_bar_remove;
  container_class->get_path_for_child = gtk_path_bar_get_path_for_child;
  gtk_container_class_handle_border_width (container_class);
  /* FIXME: */
  /*  container_class->child_type = gtk_path_bar_child_type;*/

  path_bar_signals [PATH_CLICKED] =
    g_signal_new (I_("path-clicked"),
		  G_OBJECT_CLASS_TYPE (gobject_class),
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET (GtkPathBarClass, path_clicked),
		  NULL, NULL,
		  _gtk_marshal_VOID__POINTER_POINTER_BOOLEAN,
		  G_TYPE_NONE, 3,
		  G_TYPE_POINTER,
		  G_TYPE_POINTER,
		  G_TYPE_BOOLEAN);

  /* Bind class to template
   */
  gtk_widget_class_set_template_from_resource (widget_class,
					       "/org/gtk/libgtk/ui/gtkpathbar.ui");

  gtk_widget_class_bind_template_child_private (widget_class, GtkPathBar, up_slider_button);
  gtk_widget_class_bind_template_child_private (widget_class, GtkPathBar, down_slider_button);

  gtk_widget_class_bind_template_callback (widget_class, gtk_path_bar_slider_button_press);
  gtk_widget_class_bind_template_callback (widget_class, gtk_path_bar_slider_button_release);
  gtk_widget_class_bind_template_callback (widget_class, gtk_path_bar_scroll_up);
  gtk_widget_class_bind_template_callback (widget_class, gtk_path_bar_scroll_down);
}


static void
gtk_path_bar_finalize (GObject *object)
{
  GtkPathBar *path_bar;

  path_bar = GTK_PATH_BAR (object);

  gtk_path_bar_stop_scrolling (path_bar);

  g_list_free (path_bar->priv->button_list);

  G_OBJECT_CLASS (gtk_path_bar_parent_class)->finalize (object);
}

/* Removes the settings signal handler.  It's safe to call multiple times */
static void
remove_settings_signal (GtkPathBar *path_bar,
			GdkScreen  *screen)
{
  if (path_bar->priv->settings_signal_id)
    {
      GtkSettings *settings;

      settings = gtk_settings_get_for_screen (screen);
      g_signal_handler_disconnect (settings,
				   path_bar->priv->settings_signal_id);
      path_bar->priv->settings_signal_id = 0;
    }
}

static void
gtk_path_bar_dispose (GObject *object)
{
  GtkPathBar *path_bar = GTK_PATH_BAR (object);

  remove_settings_signal (path_bar, gtk_widget_get_screen (GTK_WIDGET (object)));

  if (path_bar->priv->get_info_cancellable)
    g_cancellable_cancel (path_bar->priv->get_info_cancellable);
  path_bar->priv->get_info_cancellable = NULL;

  G_OBJECT_CLASS (gtk_path_bar_parent_class)->dispose (object);
}

/* Size requisition:
 * 
 * Ideally, our size is determined by another widget, and we are just filling
 * available space.
 */
static void
gtk_path_bar_get_preferred_width (GtkWidget *widget,
                                  gint      *minimum,
                                  gint      *natural)
{
  ButtonData *button_data;
  GtkPathBar *path_bar;
  GList *list;
  gint child_height;
  gint height;
  gint child_min, child_nat;

  path_bar = GTK_PATH_BAR (widget);

  *minimum = *natural = 0;
  height = 0;

  for (list = path_bar->priv->button_list; list; list = list->next)
    {
      button_data = BUTTON_DATA (list->data);
      gtk_widget_get_preferred_width (button_data->button, &child_min, &child_nat);
      gtk_widget_get_preferred_height (button_data->button, &child_height, NULL);
      height = MAX (height, child_height);

      if (button_data->type == NORMAL_BUTTON)
        {
          /* Use 2*Height as button width because of ellipsized label.  */
          child_min = MAX (child_min, child_height * 2);
          child_nat = MAX (child_min, child_height * 2);
        }

      *minimum = MAX (*minimum, child_min);
      *natural = *natural + child_nat;
    }

  /* Add space for slider, if we have more than one path */
  /* Theoretically, the slider could be bigger than the other button.  But we're
   * not going to worry about that now.
   */
  path_bar->priv->slider_width = MIN (height * 2 / 3 + 5, height);
  if (path_bar->priv->button_list && path_bar->priv->button_list->next != NULL)
    {
      *minimum += path_bar->priv->slider_width * 2;
      *natural += path_bar->priv->slider_width * 2;
    }
}

static void
gtk_path_bar_get_preferred_height (GtkWidget *widget,
                                   gint      *minimum,
                                   gint      *natural)
{
  ButtonData *button_data;
  GtkPathBar *path_bar;
  GList *list;
  gint child_min, child_nat;

  path_bar = GTK_PATH_BAR (widget);

  *minimum = *natural = 0;

  for (list = path_bar->priv->button_list; list; list = list->next)
    {
      button_data = BUTTON_DATA (list->data);
      gtk_widget_get_preferred_height (button_data->button, &child_min, &child_nat);

      *minimum = MAX (*minimum, child_min);
      *natural = MAX (*natural, child_nat);
    }
}

static void
gtk_path_bar_update_slider_buttons (GtkPathBar *path_bar)
{
  if (path_bar->priv->button_list)
    {
      GtkWidget *button;

      button = BUTTON_DATA (path_bar->priv->button_list->data)->button;
      if (gtk_widget_get_child_visible (button))
	{
	  gtk_path_bar_stop_scrolling (path_bar);
	  gtk_widget_set_sensitive (path_bar->priv->down_slider_button, FALSE);
	}
      else
	gtk_widget_set_sensitive (path_bar->priv->down_slider_button, TRUE);

      button = BUTTON_DATA (g_list_last (path_bar->priv->button_list)->data)->button;
      if (gtk_widget_get_child_visible (button))
	{
	  gtk_path_bar_stop_scrolling (path_bar);
	  gtk_widget_set_sensitive (path_bar->priv->up_slider_button, FALSE);
	}
      else
	gtk_widget_set_sensitive (path_bar->priv->up_slider_button, TRUE);
    }
}

static void
gtk_path_bar_map (GtkWidget *widget)
{
  gdk_window_show (GTK_PATH_BAR (widget)->priv->event_window);

  GTK_WIDGET_CLASS (gtk_path_bar_parent_class)->map (widget);
}

static void
gtk_path_bar_unmap (GtkWidget *widget)
{
  gtk_path_bar_stop_scrolling (GTK_PATH_BAR (widget));
  gdk_window_hide (GTK_PATH_BAR (widget)->priv->event_window);

  GTK_WIDGET_CLASS (gtk_path_bar_parent_class)->unmap (widget);
}

static void
gtk_path_bar_realize (GtkWidget *widget)
{
  GtkPathBar *path_bar;
  GtkAllocation allocation;
  GdkWindow *window;
  GdkWindowAttr attributes;
  gint attributes_mask;

  gtk_widget_set_realized (widget, TRUE);

  path_bar = GTK_PATH_BAR (widget);
  window = gtk_widget_get_parent_window (widget);
  gtk_widget_set_window (widget, window);
  g_object_ref (window);

  gtk_widget_get_allocation (widget, &allocation);

  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.x = allocation.x;
  attributes.y = allocation.y;
  attributes.width = allocation.width;
  attributes.height = allocation.height;
  attributes.wclass = GDK_INPUT_ONLY;
  attributes.event_mask = gtk_widget_get_events (widget);
  attributes.event_mask |= GDK_SCROLL_MASK;
  attributes_mask = GDK_WA_X | GDK_WA_Y;

  path_bar->priv->event_window = gdk_window_new (gtk_widget_get_parent_window (widget),
                                           &attributes, attributes_mask);
  gtk_widget_register_window (widget, path_bar->priv->event_window);
}

static void
gtk_path_bar_unrealize (GtkWidget *widget)
{
  GtkPathBar *path_bar;

  path_bar = GTK_PATH_BAR (widget);

  gtk_widget_unregister_window (widget, path_bar->priv->event_window);
  gdk_window_destroy (path_bar->priv->event_window);
  path_bar->priv->event_window = NULL;

  GTK_WIDGET_CLASS (gtk_path_bar_parent_class)->unrealize (widget);
}

static void
child_ordering_changed (GtkPathBar *path_bar)
{
  GList *l;

  if (path_bar->priv->up_slider_button)
    _gtk_widget_invalidate_style_context (path_bar->priv->up_slider_button,
                                          GTK_CSS_CHANGE_POSITION | GTK_CSS_CHANGE_SIBLING_POSITION);
  if (path_bar->priv->down_slider_button)
    _gtk_widget_invalidate_style_context (path_bar->priv->down_slider_button,
                                          GTK_CSS_CHANGE_POSITION | GTK_CSS_CHANGE_SIBLING_POSITION);

  for (l = path_bar->priv->button_list; l; l = l->next)
    {
      ButtonData *data = l->data;

      _gtk_widget_invalidate_style_context (data->button,
                                            GTK_CSS_CHANGE_POSITION | GTK_CSS_CHANGE_SIBLING_POSITION);
    }
}

static void
union_with_clip (GtkWidget *widget,
                 gpointer   clip)
{
  GtkAllocation widget_clip;

  if (!gtk_widget_is_drawable (widget))
    {
      return;
    }

  gtk_widget_get_clip (widget, &widget_clip);

  gdk_rectangle_union (&widget_clip, clip, clip);
}

static void
_set_simple_bottom_clip (GtkWidget *widget,
                         gint       pixels)
{
  GtkAllocation clip;

  gtk_widget_get_allocation (widget, &clip);
  clip.height += pixels;

  gtk_container_forall (GTK_CONTAINER (widget), union_with_clip, &clip);
  gtk_widget_set_clip (widget, &clip);
}

/* This is a tad complicated
 */
static void
gtk_path_bar_size_allocate (GtkWidget     *widget,
			    GtkAllocation *allocation)
{
  GtkWidget *child;
  GtkPathBar *path_bar;
  GtkTextDirection direction;
  GtkAllocation child_allocation;
  GList *list, *first_button;
  gint width;
  gint largest_width;
  gboolean need_sliders;
  gint up_slider_offset;
  gint down_slider_offset;
  GtkRequisition child_requisition;
  gboolean needs_reorder = FALSE;

  need_sliders = FALSE;
  up_slider_offset = 0;
  down_slider_offset = 0;
  path_bar = GTK_PATH_BAR (widget);

  gtk_widget_set_allocation (widget, allocation);

  if (gtk_widget_get_realized (widget))
    {
       gdk_window_move_resize (path_bar->priv->event_window,
                               allocation->x, allocation->y,
                               allocation->width, allocation->height);
    }

  /* No path is set so we don't have to allocate anything. */
  if (path_bar->priv->button_list == NULL)
    {
       _set_simple_bottom_clip (widget, BUTTON_BOTTOM_SHADOW);
       return;
    }
    direction = gtk_widget_get_direction (widget);

  /* First, we check to see if we need the scrollbars. */
  width = 0;

  gtk_widget_get_preferred_size (BUTTON_DATA (path_bar->priv->button_list->data)->button,
                                 &child_requisition, NULL);
  width += child_requisition.width;

  for (list = path_bar->priv->button_list->next; list; list = list->next)
    {
      child = BUTTON_DATA (list->data)->button;
      gtk_widget_get_preferred_size (child, &child_requisition, NULL);
      width += child_requisition.width;
    }

  if (width <= allocation->width)
    {
      first_button = g_list_last (path_bar->priv->button_list);
    }
  else
    {
      gboolean reached_end;
      gint slider_space;
      reached_end = FALSE;
      slider_space = 2 * (path_bar->priv->slider_width);

      if (path_bar->priv->first_scrolled_button)
        {
          first_button = path_bar->priv->first_scrolled_button;
        }
      else
        {
          first_button = path_bar->priv->button_list;
        }

      need_sliders = TRUE;
      /* To see how much space we have, and how many buttons we can display.
       * We start at the first button, count forward until hit the new
       * button, then count backwards.
       */
      /* Count down the path chain towards the end. */
      gtk_widget_get_preferred_size (BUTTON_DATA (first_button->data)->button,
                                     &child_requisition, NULL);
      width = child_requisition.width;
      list = first_button->prev;
      while (list && !reached_end)
        {
          child = BUTTON_DATA (list->data)->button;
          gtk_widget_get_preferred_size (child, &child_requisition, NULL);

            if (width + child_requisition.width + slider_space > allocation->width)
              {
                reached_end = TRUE;
              }
            else
              {
                width += child_requisition.width;
              }

          list = list->prev;
        }

        /* Finally, we walk up, seeing how many of the previous buttons we can add*/

      while (first_button->next && ! reached_end)
        {
          child = BUTTON_DATA (first_button->next->data)->button;
          gtk_widget_get_preferred_size (child, &child_requisition, NULL);

          if (width + child_requisition.width + slider_space > allocation->width)
            {
              reached_end = TRUE;
            }
         else
           {
             width += child_requisition.width;
             first_button = first_button->next;
           }
        }
    }

  /* Now, we allocate space to the buttons */
  child_allocation.y = allocation->y;
  child_allocation.height = allocation->height;

  if (direction == GTK_TEXT_DIR_RTL)
    {
      child_allocation.x = allocation->x + allocation->width;
      if (need_sliders)
        {
          child_allocation.x -= path_bar->priv->slider_width;
          up_slider_offset = allocation->width - path_bar->priv->slider_width;
        }
    }
  else
    {
     child_allocation.x = allocation->x;
     if (need_sliders)
       {
         up_slider_offset = 0;
         child_allocation.x += path_bar->priv->slider_width;
       }
    }

  /* Determine the largest possible allocation size */
  largest_width = allocation->width;
  if (need_sliders)
    {
      largest_width -= (path_bar->priv->slider_width) * 2;
    }

  for (list = first_button; list; list = list->prev)
    {
      child = BUTTON_DATA (list->data)->button;
      gtk_widget_get_preferred_size (child, &child_requisition, NULL);

      child_allocation.width = MIN (child_requisition.width, largest_width);
      if (direction == GTK_TEXT_DIR_RTL)
        {
          child_allocation.x -= child_allocation.width;
        }
      /* Check to see if we've don't have any more space to allocate buttons */
      if (need_sliders && direction == GTK_TEXT_DIR_RTL)
        {
          if (child_allocation.x - path_bar->priv->slider_width < allocation->x)
            {
              break;
            }
        }
      else
        {
          if (need_sliders && direction == GTK_TEXT_DIR_LTR)
            {
              if (child_allocation.x + child_allocation.width + path_bar->priv->slider_width > allocation->x + allocation->width)
                {
                  break;
                }
           }
       }

      needs_reorder |= gtk_widget_get_child_visible (child) == FALSE;
      gtk_widget_set_child_visible (child, TRUE);
      gtk_widget_size_allocate (child, &child_allocation);

      if (direction == GTK_TEXT_DIR_RTL)
        {
          down_slider_offset = child_allocation.x - allocation->x - path_bar->priv->slider_width;
        }
      else
        {
          down_slider_offset += child_allocation.width;
          child_allocation.x += child_allocation.width;
        }
    }
  /* Now we go hide all the widgets that don't fit */
  while (list)
    {
      child = BUTTON_DATA (list->data)->button;
      needs_reorder |= gtk_widget_get_child_visible (child) == TRUE;
      gtk_widget_set_child_visible (child, FALSE);
      list = list->prev;
    }
  for (list = first_button->next; list; list = list->next)
    {
      child = BUTTON_DATA (list->data)->button;
      needs_reorder |= gtk_widget_get_child_visible (child) == TRUE;
      gtk_widget_set_child_visible (child, FALSE);
    }

  if (need_sliders)
    {
      child_allocation.width = path_bar->priv->slider_width;
      child_allocation.x = up_slider_offset + allocation->x;
      gtk_widget_size_allocate (path_bar->priv->up_slider_button, &child_allocation);

      needs_reorder |= gtk_widget_get_child_visible (path_bar->priv->up_slider_button) == FALSE;
      gtk_widget_set_child_visible (path_bar->priv->up_slider_button, TRUE);
      gtk_widget_show_all (path_bar->priv->up_slider_button);

      if (direction == GTK_TEXT_DIR_LTR)
        {
          down_slider_offset += path_bar->priv->slider_width;
        }
    }
  else
    {
      needs_reorder |= gtk_widget_get_child_visible (path_bar->priv->up_slider_button) == TRUE;
      gtk_widget_set_child_visible (path_bar->priv->up_slider_button, FALSE);
    }

  if (need_sliders)
    {
      child_allocation.width = path_bar->priv->slider_width;
      child_allocation.x = down_slider_offset + allocation->x;
      gtk_widget_size_allocate (path_bar->priv->down_slider_button, &child_allocation);

      needs_reorder |= gtk_widget_get_child_visible (path_bar->priv->down_slider_button) == FALSE;
      gtk_widget_set_child_visible (path_bar->priv->down_slider_button, TRUE);
      gtk_widget_show_all (path_bar->priv->down_slider_button);
      gtk_path_bar_update_slider_buttons (path_bar);
    }
  else
    {
      needs_reorder |= gtk_widget_get_child_visible (path_bar->priv->down_slider_button) == TRUE;
      gtk_widget_set_child_visible (path_bar->priv->down_slider_button, FALSE);
    }

  if (needs_reorder)
    {
      child_ordering_changed (path_bar);
    }

  _set_simple_bottom_clip (widget, BUTTON_BOTTOM_SHADOW);
}

static void
gtk_path_bar_style_updated (GtkWidget *widget)
{
  GTK_WIDGET_CLASS (gtk_path_bar_parent_class)->style_updated (widget);

  gtk_path_bar_check_icon_theme (GTK_PATH_BAR (widget));
}

static void
gtk_path_bar_screen_changed (GtkWidget *widget,
			     GdkScreen *previous_screen)
{
  if (GTK_WIDGET_CLASS (gtk_path_bar_parent_class)->screen_changed)
    GTK_WIDGET_CLASS (gtk_path_bar_parent_class)->screen_changed (widget, previous_screen);

  /* We might nave a new settings, so we remove the old one */
  if (previous_screen)
    remove_settings_signal (GTK_PATH_BAR (widget), previous_screen);

  gtk_path_bar_check_icon_theme (GTK_PATH_BAR (widget));
}

static gboolean
gtk_path_bar_scroll (GtkWidget      *widget,
		     GdkEventScroll *event)
{
  switch (event->direction)
    {
    case GDK_SCROLL_RIGHT:
    case GDK_SCROLL_DOWN:
      gtk_path_bar_scroll_down (GTK_PATH_BAR (widget));
      break;
    case GDK_SCROLL_LEFT:
    case GDK_SCROLL_UP:
      gtk_path_bar_scroll_up (GTK_PATH_BAR (widget));
      break;
    case GDK_SCROLL_SMOOTH:
      break;
    }

  return TRUE;
}

static void
gtk_path_bar_add (GtkContainer *container,
		  GtkWidget    *widget)

{
  gtk_widget_set_parent (widget, GTK_WIDGET (container));
}

static void
gtk_path_bar_remove_1 (GtkContainer *container,
		       GtkWidget    *widget)
{
  gboolean was_visible = gtk_widget_get_visible (widget);
  gtk_widget_unparent (widget);
  if (was_visible)
    gtk_widget_queue_resize (GTK_WIDGET (container));
}

static void
gtk_path_bar_remove (GtkContainer *container,
		     GtkWidget    *widget)
{
  GtkPathBar *path_bar;
  GList *children;

  path_bar = GTK_PATH_BAR (container);

  if (widget == path_bar->priv->up_slider_button)
    {
      gtk_path_bar_remove_1 (container, widget);
      path_bar->priv->up_slider_button = NULL;
      return;
    }

  if (widget == path_bar->priv->down_slider_button)
    {
      gtk_path_bar_remove_1 (container, widget);
      path_bar->priv->down_slider_button = NULL;
      return;
    }

  children = path_bar->priv->button_list;
  while (children)
    {
      if (widget == BUTTON_DATA (children->data)->button)
	{
	  gtk_path_bar_remove_1 (container, widget);
	  path_bar->priv->button_list = g_list_remove_link (path_bar->priv->button_list, children);
	  g_list_free (children);
	  return;
	}
      
      children = children->next;
    }
}

static void
gtk_path_bar_forall (GtkContainer *container,
		     gboolean      include_internals,
		     GtkCallback   callback,
		     gpointer      callback_data)
{
  GtkPathBar *path_bar;
  GList *children;

  g_return_if_fail (callback != NULL);
  path_bar = GTK_PATH_BAR (container);

  children = path_bar->priv->button_list;
  while (children)
    {
      GtkWidget *child;
      child = BUTTON_DATA (children->data)->button;
      children = children->next;

      (* callback) (child, callback_data);
    }

  if (path_bar->priv->up_slider_button)
    (* callback) (path_bar->priv->up_slider_button, callback_data);

  if (path_bar->priv->down_slider_button)
    (* callback) (path_bar->priv->down_slider_button, callback_data);
}

static GtkWidgetPath *
gtk_path_bar_get_path_for_child (GtkContainer *container,
                                 GtkWidget    *child)
{
  GtkPathBar *path_bar = GTK_PATH_BAR (container);
  GtkWidgetPath *path;

  path = _gtk_widget_create_path (GTK_WIDGET (path_bar));

  if (gtk_widget_get_visible (child) &&
      gtk_widget_get_child_visible (child))
    {
      GtkWidgetPath *sibling_path;
      GList *visible_children;
      GList *l;
      int pos;

      /* 1. Build the list of visible children, in visually left-to-right order
       * (i.e. independently of the widget's direction).  Note that our
       * button_list is stored in innermost-to-outermost path order!
       */

      visible_children = NULL;

      if (gtk_widget_get_visible (path_bar->priv->down_slider_button) &&
          gtk_widget_get_child_visible (path_bar->priv->down_slider_button))
        visible_children = g_list_prepend (visible_children, path_bar->priv->down_slider_button);

      for (l = path_bar->priv->button_list; l; l = l->next)
        {
          ButtonData *data = l->data;

          if (gtk_widget_get_visible (data->button) &&
              gtk_widget_get_child_visible (data->button))
            visible_children = g_list_prepend (visible_children, data->button);
        }

      if (gtk_widget_get_visible (path_bar->priv->up_slider_button) &&
          gtk_widget_get_child_visible (path_bar->priv->up_slider_button))
        visible_children = g_list_prepend (visible_children, path_bar->priv->up_slider_button);

      if (gtk_widget_get_direction (GTK_WIDGET (path_bar)) == GTK_TEXT_DIR_RTL)
        visible_children = g_list_reverse (visible_children);

      /* 2. Find the index of the child within that list */

      pos = 0;

      for (l = visible_children; l; l = l->next)
        {
          GtkWidget *button = l->data;

          if (button == child)
            break;

          pos++;
        }

      /* 3. Build the path */

      sibling_path = gtk_widget_path_new ();

      for (l = visible_children; l; l = l->next)
        gtk_widget_path_append_for_widget (sibling_path, l->data);

      gtk_widget_path_append_with_siblings (path, sibling_path, pos);

      g_list_free (visible_children);
      gtk_widget_path_unref (sibling_path);
    }
  else
    gtk_widget_path_append_for_widget (path, child);

  return path;
}

static void
gtk_path_bar_scroll_down (GtkPathBar *path_bar)
{
  GtkAllocation allocation, button_allocation;
  GList *list;
  GList *down_button = NULL;
  gint space_available;

  if (path_bar->priv->ignore_click)
    {
      path_bar->priv->ignore_click = FALSE;
      return;   
    }

  if (gtk_widget_get_child_visible (BUTTON_DATA (path_bar->priv->button_list->data)->button))
    {
      /* Return if the last button is already visible */
      return;
    }

  gtk_widget_queue_resize (GTK_WIDGET (path_bar));

  /* We find the button at the 'down' end that we have to make
   * visible */
  for (list = path_bar->priv->button_list; list; list = list->next)
    {
      if (list->next && gtk_widget_get_child_visible (BUTTON_DATA (list->next->data)->button))
	{
	  down_button = list;
	  break;
	}
    }

  gtk_widget_get_allocation (GTK_WIDGET (path_bar), &allocation);
  gtk_widget_get_allocation (BUTTON_DATA (down_button->data)->button, &button_allocation);

  space_available = (allocation.width
		     - 2 * path_bar->priv->slider_width
                     - button_allocation.width);
  path_bar->priv->first_scrolled_button = down_button;
  
  /* We have space_available free space that's not being used.  
   * So we walk down from the end, adding buttons until we use all free space.
   */
  while (space_available > 0)
    {
      path_bar->priv->first_scrolled_button = down_button;
      down_button = down_button->next;
      if (!down_button)
	break;
      space_available -= button_allocation.width;
    }
}

static void
gtk_path_bar_scroll_up (GtkPathBar *path_bar)
{
  GList *list;

  if (path_bar->priv->ignore_click)
    {
      path_bar->priv->ignore_click = FALSE;
      return;   
    }

  list = g_list_last (path_bar->priv->button_list);

  if (gtk_widget_get_child_visible (BUTTON_DATA (list->data)->button))
    {
      /* Return if the first button is already visible */
      return;
    }

  gtk_widget_queue_resize (GTK_WIDGET (path_bar));

  for ( ; list; list = list->prev)
    {
      if (list->prev && gtk_widget_get_child_visible (BUTTON_DATA (list->prev->data)->button))
	{
	  path_bar->priv->first_scrolled_button = list;
	  return;
	}
    }
}

static gboolean
gtk_path_bar_scroll_timeout (GtkPathBar *path_bar)
{
  gboolean retval = FALSE;

  if (path_bar->priv->timer)
    {
      if (path_bar->priv->scrolling_up)
        gtk_path_bar_scroll_up (path_bar);
      else if (path_bar->priv->scrolling_down)
        gtk_path_bar_scroll_down (path_bar);

      if (path_bar->priv->need_timer) 
        {
          path_bar->priv->need_timer = FALSE;

          path_bar->priv->timer = g_timeout_add (SCROLL_TIMEOUT,
                                                (GSourceFunc)gtk_path_bar_scroll_timeout,
                                                path_bar);
        }
      else
      retval = TRUE;
    }

  return retval;
}

static void 
gtk_path_bar_stop_scrolling (GtkPathBar *path_bar)
{
  if (path_bar->priv->timer)
    {
      g_source_remove (path_bar->priv->timer);
      path_bar->priv->timer = 0;
      path_bar->priv->need_timer = FALSE;
    }
}

static gboolean
gtk_path_bar_slider_button_press (GtkWidget      *widget, 
				  GdkEventButton *event,
				  GtkPathBar     *path_bar)
{
  if (event->type != GDK_BUTTON_PRESS || event->button != GDK_BUTTON_PRIMARY)
    return FALSE;

  path_bar->priv->ignore_click = FALSE;

  if (widget == path_bar->priv->up_slider_button)
    {
      path_bar->priv->scrolling_down = FALSE;
      path_bar->priv->scrolling_up = TRUE;
      gtk_path_bar_scroll_up (path_bar);
    }
  else if (widget == path_bar->priv->down_slider_button)
    {
      path_bar->priv->scrolling_up = FALSE;
      path_bar->priv->scrolling_down = TRUE;
      gtk_path_bar_scroll_down (path_bar);
    }

  if (!path_bar->priv->timer)
    {
      path_bar->priv->need_timer = TRUE;
      path_bar->priv->timer =  g_timeout_add (INITIAL_SCROLL_TIMEOUT,
                                              (GSourceFunc) gtk_path_bar_scroll_timeout,
                                              path_bar);
    }

  return FALSE;
}

static gboolean
gtk_path_bar_slider_button_release (GtkWidget      *widget, 
				    GdkEventButton *event,
				    GtkPathBar     *path_bar)
{
  if (event->type != GDK_BUTTON_RELEASE)
    return FALSE;

  path_bar->priv->ignore_click = TRUE;
  gtk_path_bar_stop_scrolling (path_bar);

  return FALSE;
}

static void
gtk_path_bar_grab_notify (GtkWidget *widget,
			  gboolean   was_grabbed)
{
  if (!was_grabbed)
    gtk_path_bar_stop_scrolling (GTK_PATH_BAR (widget));
}

static void
gtk_path_bar_state_changed (GtkWidget    *widget,
			    GtkStateType  previous_state)
{
  if (!gtk_widget_is_sensitive (widget))
    gtk_path_bar_stop_scrolling (GTK_PATH_BAR (widget));
}


/* Changes the icons wherever it is needed */
static void
reload_icons (GtkPathBar *path_bar)
{
  GList *list;

  for (list = path_bar->priv->button_list; list; list = list->next)
    {
      ButtonData *button_data;

      button_data = BUTTON_DATA (list->data);
      if (button_data->type != NORMAL_BUTTON || button_data->is_root)
	{
	  gtk_path_bar_update_button_appearance (button_data);
	}
    }
  
}

static void
change_icon_theme (GtkPathBar *path_bar)
{
  gint width, height;

  if (gtk_icon_size_lookup (GTK_ICON_SIZE_MENU, &width, &height))
    path_bar->priv->icon_size = MAX (width, height);
  else
    path_bar->priv->icon_size = FALLBACK_ICON_SIZE;

  reload_icons (path_bar);
}

/* Callback used when a GtkSettings value changes */
static void
settings_notify_cb (GObject    *object,
		    GParamSpec *pspec,
		    GtkPathBar *path_bar)
{
  const char *name;

  name = g_param_spec_get_name (pspec);

  if (strcmp (name, "gtk-icon-theme-name") == 0)
    change_icon_theme (path_bar);
}

static void
gtk_path_bar_check_icon_theme (GtkPathBar *path_bar)
{
  if (path_bar->priv->settings_signal_id == 0)
    {
      GtkSettings *settings;

      settings = gtk_settings_get_for_screen (gtk_widget_get_screen (GTK_WIDGET (path_bar)));
      path_bar->priv->settings_signal_id = g_signal_connect (settings, "notify",
                                                             G_CALLBACK (settings_notify_cb), path_bar);
    }

  change_icon_theme (path_bar);
}

/* Public functions and their helpers */
static void
gtk_path_bar_clear_buttons (GtkPathBar *path_bar)
{
  while (path_bar->priv->button_list != NULL)
    {
      gtk_container_remove (GTK_CONTAINER (path_bar), BUTTON_DATA (path_bar->priv->button_list->data)->button);
    }
  path_bar->priv->first_scrolled_button = NULL;
}

static void
button_clicked_cb (GtkWidget *button,
		   gpointer   data)
{
  ButtonData *button_data;
  GtkPathBar *path_bar;
  GList *button_list;
  gboolean child_is_hidden;
  GFile *child_file;

  button_data = BUTTON_DATA (data);
  if (button_data->ignore_changes)
    return;

  path_bar = GTK_PATH_BAR (gtk_widget_get_parent (button));

  button_list = g_list_find (path_bar->priv->button_list, button_data);
  g_assert (button_list != NULL);

  g_signal_handlers_block_by_func (button,
				   G_CALLBACK (button_clicked_cb), data);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
  g_signal_handlers_unblock_by_func (button,
				     G_CALLBACK (button_clicked_cb), data);

  if (button_list->prev)
    {
      ButtonData *child_data;

      child_data = BUTTON_DATA (button_list->prev->data);
      child_file = child_data->file;
      child_is_hidden = child_data->file_is_hidden;
    }
  else
    {
      child_file = NULL;
      child_is_hidden = FALSE;
    }

  g_print ("button clicked %s\n", g_file_get_uri (button_data->file));

  g_signal_emit (path_bar, path_bar_signals [PATH_CLICKED], 0,
		 button_data->file, child_file, child_is_hidden);
}

struct SetButtonImageData
{
  GtkPathBar *path_bar;
  ButtonData *button_data;
};

GMount *
gtk_path_bar_get_mounted_mount_for_root (GFile *location)
{
  GVolumeMonitor *volume_monitor;
  GList *mounts;
  GList *l;
  GMount *mount;
  GMount *result = NULL;
  GFile *root = NULL;
  GFile *default_location = NULL;

  volume_monitor = g_volume_monitor_get ();
  mounts = g_volume_monitor_get_mounts (volume_monitor);

  for (l = mounts; l != NULL; l = l->next)
    {
      mount = l->data;

      if (g_mount_is_shadowed (mount))
        {
          continue;
        }

      root = g_mount_get_root (mount);
      if (g_file_equal (location, root))
        {
          result = g_object_ref (mount);
          break;
        }

      default_location = g_mount_get_default_location (mount);
      if (!g_file_equal (default_location, root) &&
          g_file_equal (location, default_location))
        {
          result = g_object_ref (mount);
          break;
        }
    }

  g_clear_object (&root);
  g_clear_object (&default_location);
  g_list_free_full (mounts, g_object_unref);

  return result;
}

static GIcon *
get_gicon_for_mount (ButtonData *button_data)
{
  GIcon *icon;
  GMount *mount;

  icon = NULL;
  mount = gtk_path_bar_get_mounted_mount_for_root (button_data->file);

  if (mount != NULL)
    {
      icon = g_mount_get_symbolic_icon (mount);
      g_object_unref (mount);
    }

  return icon;
}

static GIcon *
get_gicon (ButtonData *button_data)
{
  switch (button_data->type)
    {
      case ROOT_BUTTON:
        return g_themed_icon_new ("drive-harddisk-symbolic");
      case HOME_BUTTON:
        return g_themed_icon_new ("user-home-symbolic");
      case DESKTOP_BUTTON:
        return g_themed_icon_new ("user-desktop-symbolic");
      case MOUNT_BUTTON:
        return get_gicon_for_mount (button_data);
      default:
        return NULL;
    }

  return NULL;
}

static void
button_data_free (ButtonData *button_data)
{
  if (button_data->file)
    {
      g_signal_handler_disconnect (button_data->monitor,
                                   button_data->file_changed_signal_id);
      button_data->file_changed_signal_id = 0;
      g_object_unref (button_data->monitor);
      g_object_unref (button_data->file);
      button_data->file = NULL;
      button_data->monitor= NULL;
    }
  button_data->file = NULL;

  g_free (button_data->dir_name);
  button_data->dir_name = NULL;

  g_print ("freeing button\n");
  button_data->button = NULL;

  if (button_data->cancellable)
    g_cancellable_cancel (button_data->cancellable);
  else
    g_free (button_data);
}

static const char *
get_dir_name (ButtonData *button_data)
{
  return button_data->dir_name;
}

static void
gtk_path_bar_update_button_appearance (ButtonData *button_data)
{
  const gchar *dir_name = get_dir_name (button_data);
  GIcon *icon;

  if (button_data->label != NULL)
      gtk_label_set_text (GTK_LABEL (button_data->label), dir_name);

  icon = get_gicon (button_data);
  if (icon != NULL)
    {
      gtk_image_set_from_gicon (GTK_IMAGE (button_data->image), icon, GTK_ICON_SIZE_MENU);
      gtk_widget_show (GTK_WIDGET (button_data->image));
      g_object_unref (icon);
    }
  else
    {
      gtk_widget_hide (GTK_WIDGET (button_data->image));
    }
}

static void
gtk_path_bar_update_button_appearance_and_state (ButtonData *button_data,
				                 gboolean    current_dir)
{
	gtk_path_bar_update_button_appearance (button_data);

        if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (button_data->button)) != current_dir) {
                button_data->ignore_changes = TRUE;
                gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button_data->button), current_dir);
                button_data->ignore_changes = FALSE;
        }
}

gboolean
gtk_path_bar_is_home_directory (GFile *dir)
{
  static GFile *home_dir = NULL;

  if (home_dir == NULL)
    {
      home_dir = g_file_new_for_path (g_get_home_dir ());
    }

  return g_file_equal (dir, home_dir);
}

gboolean
gtk_path_bar_is_root_directory (GFile *dir)
{
  static GFile *root_dir = NULL;

  if (root_dir == NULL)
    {
      root_dir = g_file_new_for_path ("/");
    }

  return g_file_equal (dir, root_dir);
}

gboolean
gtk_path_bar_is_desktop_directory (GFile *dir)
{
  static GFile *desktop_dir = NULL;
  const char *desktop_path;

  if (desktop_dir == NULL)
    {
      desktop_path = g_get_user_special_dir (G_USER_DIRECTORY_DESKTOP);
	    if (desktop_path == NULL)
        {
          desktop_path = g_get_home_dir ();
        }
      desktop_dir = g_file_new_for_path (desktop_path);
    }

  return g_file_equal (dir, desktop_dir);
}

static void
gtk_path_bar_setup_button_type (ButtonData *button_data,
                                GFile      *location)
{
  GMount *mount;

  if (gtk_path_bar_is_root_directory (location))
    {
      button_data->type = ROOT_BUTTON;
    }
  else if (gtk_path_bar_is_home_directory (location))
    {
      button_data->type = HOME_BUTTON;
      button_data->is_root = TRUE;
    }
  else if (gtk_path_bar_is_desktop_directory (location))
    {
      button_data->type = DESKTOP_BUTTON;
    }
  else if ((mount = gtk_path_bar_get_mounted_mount_for_root (location)) != NULL)
    {
      button_data->dir_name = g_mount_get_name (mount);
      button_data->type = MOUNT_BUTTON;
      button_data->is_root = TRUE;

      g_object_unref (mount);
    }
  else
    {
      button_data->type = NORMAL_BUTTON;
    }
}

static void
button_drag_data_get_cb (GtkWidget        *widget,
                         GdkDragContext   *context,
                         GtkSelectionData *selection_data,
                         guint             info,
                         guint             time_,
                         gpointer          data)
{
  ButtonData *button_data;
  char *uris[2];

  button_data = data;

  uris[0] = g_file_get_uri (button_data->file);
  uris[1] = NULL;

  gtk_selection_data_set_uris (selection_data, uris);

  g_free (uris[0]);
}

static ButtonData *
gtk_path_bar_find_button_from_file (GtkPathBar *path_bar,
                                    GFile      *file)
{
  ButtonData *result = NULL;
  ButtonData *button_data;
  GList *list;

  g_return_val_if_fail (G_IS_FILE (file), NULL);

  for (list = path_bar->priv->button_list; list; list = list->next)
    {
      button_data = list->data;
      if (g_file_equal (file, button_data->file))
        {
          result = list->data;
          break;
        }
    }

  return result;
}

static ButtonData *
gtk_path_bar_get_button_child (GtkPathBar *path_bar,
                               ButtonData *button_data)
{
  ButtonData *result = NULL;
  GList *list;

  g_return_val_if_fail (button_data != NULL, NULL);

  list = g_list_find (path_bar->priv->button_list, button_data);
  result = list->prev != NULL? list->prev->data : NULL;

  return result;
}

static void
gtk_path_bar_button_data_unmonitor_file (ButtonData *button_data)
{
  g_return_if_fail (button_data != NULL);

  if (button_data->file)
    {
      g_signal_handler_disconnect (button_data->monitor,
                                   button_data->file_changed_signal_id);
      button_data->file_changed_signal_id = 0;
      g_object_unref (button_data->monitor);
      g_object_unref (button_data->file);
      button_data->file = NULL;
      button_data->monitor= NULL;
    }
}

static void
gtk_path_bar_button_data_monitor_file (ButtonData *button_data)
{
  g_return_if_fail (button_data != NULL);

  button_data->monitor = g_file_monitor (button_data->file, G_FILE_MONITOR_SEND_MOVED, NULL, NULL);
  button_data->file_changed_signal_id = g_signal_connect (button_data->monitor, "changed",
		                                                      G_CALLBACK (gtk_path_bar_on_file_changed),
		                                                      button_data);
}

static void
gtk_path_bar_remove_dangling_buttons (GtkPathBar *path_bar,
                                      ButtonData *button_data,
                                      GFile      *new_file)
{
  GList *new_list;
  GList *l;
  GList *next;
  GFile *new_parent;

  l = g_list_find (path_bar->priv->button_list, button_data);
  g_return_if_fail (l != NULL);
  new_parent = g_file_get_parent (new_file);

  /* Start in the next outermost button after the one that actually changed */
  l = l->next;
  while (l != NULL)
    {
      g_print ("while %s, %s\n", g_file_get_uri (BUTTON_DATA (l->data)->file), g_file_get_uri (new_parent));
      if (g_file_has_prefix (BUTTON_DATA (l->data)->file, new_parent))
        {
          next = l->next;
          g_print ("inside\n");
          new_list = g_list_remove (path_bar->priv->button_list, l->data);
          l = next;
        }
      else
        {
          child_ordering_changed (path_bar);
          break;
        }
    }

  g_object_unref (new_parent);
}

typedef struct  _UpdatePathData {
  GFile *updated_file;
  ButtonData *button_data;
} UpdatePathData;

static void
update_path_data_free (UpdatePathData *update_path_data)
{
  /* Doesn't free the button data, since that's managed
   * by the GtkPathBar */
  g_object_unref (update_path_data->updated_file);
  g_slice_free (UpdatePathData, update_path_data);
}

static void
gtk_path_bar_on_file_finished (GObject      *object,
                               GAsyncResult *result,
                               gpointer      user_data)
{
  g_print ("callback called!\n");
  GtkPathBar *path_bar;

  path_bar = GTK_PATH_BAR (object);
}

static void
gtk_path_bar_update_path_on_get_info (GObject      *object,
                                      GAsyncResult *result,
                                      gpointer      user_data)
{
  const gchar *display_name;
  ButtonData *button_data;
  ButtonData *child_button_data;
  UpdatePathData *update_path_data;
  UpdatePathData *new_update_path_data;
  GTask *task;
  GtkPathBar *path_bar;
  GFileInfo *info;
  GFile *file;
  gchar *child_basename;
  GFile *updated_child_file;


  task = (GTask*) user_data;
  update_path_data = (UpdatePathData *) g_task_get_task_data (task);
  button_data = update_path_data->button_data;
  file = G_FILE (object);
  g_print ("before button data %s\n", g_file_get_uri (file));
  info = g_file_query_info_finish (file, result, NULL);
  path_bar = GTK_PATH_BAR (g_task_get_source_object (task));

  g_return_if_fail (info != NULL);
  g_return_if_fail (path_bar != NULL);

  display_name = g_file_info_get_display_name (info);

  if (g_strcmp0 (display_name, button_data->dir_name) != 0)
    {
      g_free (button_data->dir_name);
      button_data->dir_name = g_strdup (display_name);
    }
  g_print ("in get info\n");
  gtk_path_bar_update_button_appearance (button_data);
  g_print ("updated appearance\n");

  /* Update recursively if there is a child */
  child_button_data = gtk_path_bar_get_button_child (path_bar, button_data);
  g_print ("child data ok\n");
  if (child_button_data != NULL)
    {
      /* Set the children button data for operating on the next button */
      new_update_path_data = g_slice_new (UpdatePathData);
      child_basename = g_file_get_basename (child_button_data->file);
      updated_child_file = g_file_get_child (button_data->file, child_basename);
      new_update_path_data->updated_file = g_object_ref (updated_child_file);
      new_update_path_data->button_data = child_button_data;
      g_task_set_task_data (task, new_update_path_data,
                            (GDestroyNotify) update_path_data_free);

      gtk_path_bar_update_path_async (task);
    }
  else
    {
      g_print ("null ended\n");
      /* No more button, we finished */
      g_task_return_pointer (task, NULL, NULL);
    }

  g_object_unref (info);
}

static void
gtk_path_bar_update_path_async (GTask *task)
{
  ButtonData *button_data;
  UpdatePathData *update_path_data;
  GtkPathBar *path_bar;

  update_path_data = (UpdatePathData *) g_task_get_task_data (task);
  path_bar = GTK_PATH_BAR (g_task_get_source_object (task));

  button_data = BUTTON_DATA (update_path_data->button_data);

  /* Replace the old file with the new one */
  if (g_file_equal (path_bar->priv->current_path, button_data->file))
    {
      path_bar->priv->current_path = update_path_data->updated_file;
    }
  gtk_path_bar_button_data_unmonitor_file (button_data);
  button_data->file= g_object_ref (update_path_data->updated_file);
  gtk_path_bar_button_data_monitor_file (button_data);

  g_print ("asking info for %s\n", g_file_get_uri (button_data->file));
  /* Ask for the user friendly name (display-name) and update the button label */
  g_file_query_info_async (button_data->file,
                           "standard::display-name",
                           G_FILE_QUERY_INFO_NONE,
                           G_PRIORITY_DEFAULT,
                           NULL,
                           gtk_path_bar_update_path_on_get_info,
                           task);
}

static void
gtk_path_bar_update_path (GtkPathBar          *path_bar,
                          ButtonData          *button_data,
                          GFile               *updated_file,
                          GAsyncReadyCallback  callback)
{
  GTask *task;
  UpdatePathData *update_path_data;

  update_path_data = g_slice_new (UpdatePathData);
  update_path_data->updated_file = g_object_ref (updated_file);
  update_path_data->button_data = button_data;

  task = g_task_new (path_bar, NULL, callback, update_path_data);
  g_task_set_task_data (task, update_path_data,
                        (GDestroyNotify) update_path_data_free);

  gtk_path_bar_update_path_async (task);
}

static void
gtk_path_bar_on_file_changed (GFileMonitor      *monitor,
                              GFile             *file,
                              GFile             *new_file,
                              GFileMonitorEvent  event_type,
                              gpointer          *user_data)
{
  GFile *new_file_parent, *old_file_parent;
  ButtonData *button_data;
  ButtonData *innermost_button_data;
  GFile *innermost_path;
  GtkPathBar *path_bar;
  gboolean renamed;

  button_data = BUTTON_DATA (user_data);
  path_bar = (GtkPathBar*) gtk_widget_get_ancestor (button_data->button,
                                                    GTK_TYPE_PATH_BAR);
  g_print ("event type %i\n", event_type);
  g_print ("gtk_path_bar_on_file_changed %s, %s, %s\n", g_file_get_uri (file), g_file_get_uri (button_data->file), g_file_get_uri (path_bar->priv->current_path));
  if (path_bar == NULL)
    {
      return;
    }

  g_assert (path_bar->priv->current_path!= NULL);

  innermost_button_data = path_bar->priv->button_list->data;
  innermost_path = innermost_button_data->file;

  /* If the change doesn't affect directly any file in the path bar, don't do anything.
   * This happens when one of the random children files of one of the folders in
   * the path bar is modified, but that modified children file is not a children
   * in the hierarchy of the pathbar. */
  if (!g_file_has_prefix (innermost_path, file) && !g_file_equal (file, innermost_path))
    return;

  if (event_type == G_FILE_MONITOR_EVENT_MOVED)
    {

      old_file_parent = g_file_get_parent (file);
      new_file_parent = g_file_get_parent (new_file);
      g_print ("new file %s\n", g_file_get_uri (new_file));

      renamed = (old_file_parent != NULL && new_file_parent != NULL) &&
                 g_file_equal (old_file_parent, new_file_parent);

      g_clear_object (&old_file_parent);
      g_clear_object (&new_file_parent);

      if (renamed)
        {
          g_print ("renamed \n");
          /* 1- Rename buttons from the modified one to the innermost button */
          ButtonData *renamed_button_data;

          renamed_button_data = gtk_path_bar_find_button_from_file (path_bar, file);
          g_return_if_fail (renamed_button_data != NULL);
          gtk_path_bar_update_path (path_bar, renamed_button_data, new_file,
                                   (GAsyncReadyCallback) gtk_path_bar_on_file_finished);
        }
      else
        {
          if (g_file_has_prefix (file, path_bar->priv->current_path))
            {
              /* Moved file below the current path. We want to just remove the
               * buttons below the moved file since they are in a diferent
               * hierarchy now. To do:
               * 1- Remove the dangling buttons, those are the buttons that are
               * below the one that is moved */
              g_print ("moved inside current path\n");
            }
          else
            {
              /* Moved file above the current path. To do:
               * 1- Remove the dangling buttons, those are the buttons that are
               * parents of the old file and are no longer children of the old file
               * parent.
               * 2- Rename buttons from the modified one to the innermost button */
              ButtonData *renamed_button_data;

              renamed_button_data = gtk_path_bar_find_button_from_file (path_bar, file);
              g_assert (renamed_button_data != NULL);
              gtk_path_bar_remove_dangling_buttons (path_bar, renamed_button_data, new_file);
              gtk_path_bar_update_path (path_bar, renamed_button_data, new_file,
                                        (GAsyncReadyCallback) gtk_path_bar_on_file_finished);
              //g_object_unref (path_bar->priv->current_path);
              //path_bar->priv->current_path = g_object_ref (new_file);
              //_gtk_path_bar_set_file (path_bar, current_path, FALSE);
              g_print ("moved outside current path\n");
            }

          return;
        }
    }
#if 0
  else if (event_type == G_FILE_MONITOR_EVENT_DELETED)
    {
      gint idx, position;
      /* if the current or a parent location are gone, clear all the buttons,
       * the view will set the new path.
       */
      if (g_file_has_prefix (path_bar->priv->current_path, file) ||
          g_file_equal (path_bar->priv->current_path, file))
        {
          g_print ("deleted below current\n");
          gtk_path_bar_clear_buttons (path_bar);
        }
      else if (g_file_has_prefix (file, path_bar->priv->current_path))
        {
          g_print ("deleted below current\n");
          /* remove this and the following buttons */
          position = g_list_position (path_bar->priv->button_list,
                     g_list_find (path_bar->priv->button_list, button_data));

          if (position != -1)
            {
              for (idx = 0; idx <= position; idx++)
                {
                  gtk_container_remove (GTK_CONTAINER (path_bar),
                                        BUTTON_DATA (path_bar->priv->button_list->data)->button);
                }
            }
        }


      return;
    }

#endif

  /* MOUNTs use the GMount as the name, so don't update for those */
}

static ButtonData *
make_directory_button (GtkPathBar  *path_bar,
		       const char  *dir_name,
		       GFile       *file,
		       gboolean     current_dir,
		       gboolean     file_is_hidden)
{
  AtkObject *atk_obj;
  GtkWidget *child = NULL;
  ButtonData *button_data;

  file_is_hidden = !! file_is_hidden;
  /* Is it a special button? */
  button_data = g_new0 (ButtonData, 1);

  gtk_path_bar_setup_button_type (button_data, file);
  button_data->button = gtk_toggle_button_new ();
  atk_obj = gtk_widget_get_accessible (button_data->button);
  gtk_button_set_focus_on_click (GTK_BUTTON (button_data->button), FALSE);
  gtk_widget_add_events (button_data->button, GDK_SCROLL_MASK);

  button_data->image = gtk_image_new ();
  switch (button_data->type)
    {
    case ROOT_BUTTON:
      child = button_data->image;
      button_data->label = NULL;
      atk_object_set_name (atk_obj, _("File System Root"));
      break;
    case DESKTOP_BUTTON:
    case HOME_BUTTON:
    case MOUNT_BUTTON:
    case NORMAL_BUTTON:
    default:
      button_data->label = gtk_label_new (NULL);
      child = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
      gtk_box_pack_start (GTK_BOX (child), button_data->image, FALSE, FALSE, 0);
      gtk_box_pack_start (GTK_BOX (child), button_data->label, FALSE, FALSE, 0);
      break;
    }

  button_data->dir_name = g_strdup (dir_name);
  button_data->file = g_object_ref (file);
  button_data->file_is_hidden = file_is_hidden;

  button_data->monitor = g_file_monitor (file, G_FILE_MONITOR_SEND_MOVED, NULL, NULL);
  button_data->file_changed_signal_id = g_signal_connect (button_data->monitor, "changed",
		                                          G_CALLBACK (gtk_path_bar_on_file_changed),
		                                          button_data);

  gtk_container_add (GTK_CONTAINER (button_data->button), child);
  gtk_widget_show_all (button_data->button);

  gtk_path_bar_update_button_appearance_and_state (button_data, current_dir);

  g_signal_connect (button_data->button, "clicked",
		    G_CALLBACK (button_clicked_cb),
		    button_data);
  g_object_weak_ref (G_OBJECT (button_data->button),
		     (GWeakNotify) button_data_free, button_data);

  gtk_drag_source_set (button_data->button,
		       GDK_BUTTON1_MASK,
		       NULL, 0,
		       GDK_ACTION_COPY);
  gtk_drag_source_add_uri_targets (button_data->button);
  g_signal_connect (button_data->button, "drag-data-get",
		    G_CALLBACK (button_drag_data_get_cb), button_data);

  return button_data;
}

static gboolean
gtk_path_bar_check_parent_path (GtkPathBar         *path_bar,
				GFile              *file)
{
  GList *list;
  GList *current_path = NULL;

  for (list = path_bar->priv->button_list; list; list = list->next)
    {
      ButtonData *button_data;

      button_data = list->data;
      if (g_file_equal (file, button_data->file))
	{
	  current_path = list;
	  break;
	}
    }

  if (current_path)
    {
      for (list = path_bar->priv->button_list; list; list = list->next)
	{
	  gtk_path_bar_update_button_appearance_and_state (BUTTON_DATA (list->data),
                                                           (list == current_path) ? TRUE : FALSE);
	}

      if (!gtk_widget_get_child_visible (BUTTON_DATA (current_path->data)->button))
	{
	  path_bar->priv->first_scrolled_button = current_path;
	  gtk_widget_queue_resize (GTK_WIDGET (path_bar));
	}

      return TRUE;
    }
  return FALSE;
}


struct SetFileInfo
{
  GFile *file;
  GFile *parent_file;
  GtkPathBar *path_bar;
  GList *new_buttons;
  gboolean first_directory;
};

static void
gtk_path_bar_set_file_finish (struct SetFileInfo *info,
                              gboolean            result)
{
  if (result)
    {
      GList *l;

      gtk_path_bar_clear_buttons (info->path_bar);
      info->path_bar->priv->button_list = g_list_reverse (info->new_buttons);

      for (l = info->path_bar->priv->button_list; l; l = l->next)
	{
	  GtkWidget *button = BUTTON_DATA (l->data)->button;
	  gtk_container_add (GTK_CONTAINER (info->path_bar), button);
	}

      child_ordering_changed (info->path_bar);
    }
  else
    {
      GList *l;

      for (l = info->new_buttons; l; l = l->next)
	{
	  ButtonData *button_data;

	  button_data = BUTTON_DATA (l->data);
	  g_print ("freeing button\n");
	  gtk_widget_destroy (button_data->button);
	}

      g_list_free (info->new_buttons);
    }

  if (info->file)
    g_object_unref (info->file);
  if (info->parent_file)
    g_object_unref (info->parent_file);

  g_free (info);
}

#if 0
static void
gtk_path_bar_update_path (GtkPathBar *path_bar,
                          GFile      *file)
{
  gboolean first_directory;
  GList *new_buttons, *l;
  ButtonData *button_data;

  g_return_if_fail (NAUTILUS_IS_PATH_BAR (path_bar));
  g_return_if_fail (file_path != NULL);

  selected_directory = TRUE;
  new_buttons = NULL;

  while (file != NULL)
    {
      GFile *parent_file;

      parent_file = g_file_get_parent (file);
      button_data = make_button_data (path_bar, file, first_directory);
      g_object_unref (file);

      if (selected_directory)
        {
          selected_directory = FALSE;
        }

      new_buttons = g_list_prepend (new_buttons, button_data);

      if (parent_file != NULL && button_data->is_root)
        {
          nautilus_file_unref (parent_file);
          break;
		    }

      file = parent_file;
     }

  gtk_path_bar_clear_buttons (path_bar);
  path_bar->priv->button_list = g_list_reverse (new_buttons);

  for (l = path_bar->priv->button_list; l; l = l->next)
    {
      GtkWidget *button;
      button = BUTTON_DATA (l->data)->button;
      gtk_container_add (GTK_CONTAINER (path_bar), button);
    }

	child_ordering_changed (path_bar);
}

static void
gtk_path_bar_fetch_info (GtkPathBar *path_bar,
                         GFile      *file)
{
  struct SetFileInfo *info;

  info = g_new0 (struct SetFileInfo, 1);
  info->file = g_object_ref (file);
  info->path_bar = path_bar;
  info->first_directory = TRUE;
  info->parent_file = g_file_get_parent (info->file);

  g_file_query_info_async (file,
                           "standard::display-name,standard::is-hidden,standard::is-backup",
                           G_FILE_QUERY_INFO_NONE,
                           G_PRIORITY_DEFAULT,
                           path_bar->priv->get_info_cancellable,
                           gtk_path_bar_get_info_callback,
                           info);
}
#endif

static void
gtk_path_bar_get_info_callback (GObject      *source,
                                GAsyncResult *result,
                                gpointer      data)
{
  GFile *file = G_FILE (source);
  GFileInfo *info;
  GError *error;
  struct SetFileInfo *file_info = data;
  ButtonData *button_data;
  const gchar *display_name;
  gboolean is_hidden;

  info = g_file_query_info_finish (file, result, &error);

  if (g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED) || !info)
    {
      g_object_unref (file_info->path_bar->priv->get_info_cancellable);
      file_info->path_bar->priv->get_info_cancellable = NULL;
      gtk_path_bar_set_file_finish (file_info, FALSE);
      goto out;
    }

  display_name = g_file_info_get_display_name (info);
  is_hidden = g_file_info_get_is_hidden (info) || g_file_info_get_is_backup (info);

  button_data = make_directory_button (file_info->path_bar, display_name,
                                       file_info->file,
                                       file_info->first_directory, is_hidden);
  g_object_unref (file_info->file);

  file_info->new_buttons = g_list_prepend (file_info->new_buttons, button_data);

  /* We have assigned the info for the innermost button, i.e. the deepest directory.
   * Now, go on to fetch the info for this directory's parent.
   */
  file_info->file = file_info->parent_file;
  file_info->parent_file= NULL;
  file_info->first_directory = FALSE;

  if (!file_info->file || button_data->is_root)
    {
      /* No parent?  Okay, we are done. */
      gtk_path_bar_set_file_finish (file_info, TRUE);
      goto out;
    }

  file_info->parent_file = g_file_get_parent (file_info->file);

  /* Recurse asynchronously */
  g_file_query_info_async (file_info->file,
                           "standard::display-name,standard::is-hidden,standard::is-backup",
                           G_FILE_QUERY_INFO_NONE,
                           G_PRIORITY_DEFAULT,
                           file_info->path_bar->priv->get_info_cancellable,
                           gtk_path_bar_get_info_callback,
                           file_info);
out:
  g_object_unref (info);
}

void
_gtk_path_bar_set_file (GtkPathBar *path_bar,
                        GFile      *file,
                        gboolean    keep_trail)
{
  struct SetFileInfo *info;

  g_return_if_fail (GTK_IS_PATH_BAR (path_bar));
  g_return_if_fail (G_IS_FILE (file));

  if (path_bar->priv->current_path != NULL)
    {
      //g_object_unref (path_bar->priv->current_path);
    }
  path_bar->priv->current_path = g_object_ref (file);

  g_print ("set file %s\n", g_file_get_uri (file));
  /* Check whether the new path is already present in the pathbar as buttons.
   * This could be a parent directory or a previous selected subdirectory.
   */
  if (keep_trail && gtk_path_bar_check_parent_path (path_bar, file))
    return;

  info = g_new0 (struct SetFileInfo, 1);
  info->file = g_object_ref (file);
  info->path_bar = path_bar;
  info->first_directory = TRUE;
  info->parent_file = g_file_get_parent (info->file);

  if (path_bar->priv->get_info_cancellable)
    {
      g_cancellable_cancel (path_bar->priv->get_info_cancellable);
      g_object_unref (path_bar->priv->get_info_cancellable);
    }

  path_bar->priv->get_info_cancellable = g_cancellable_new ();

  g_file_query_info_async (file,
                           "standard::display-name,standard::is-hidden,standard::is-backup",
                           G_FILE_QUERY_INFO_NONE,
                           G_PRIORITY_DEFAULT,
                           path_bar->priv->get_info_cancellable,
                           gtk_path_bar_get_info_callback,
                           info);
}

/**
 * _gtk_path_bar_up:
 * @path_bar: a #GtkPathBar
 * 
 * If the selected button in the pathbar is not the furthest button “up” (in the
 * root direction), act as if the user clicked on the next button up.
 **/
void
_gtk_path_bar_up (GtkPathBar *path_bar)
{
  GList *l;

  for (l = path_bar->priv->button_list; l; l = l->next)
    {
      GtkWidget *button = BUTTON_DATA (l->data)->button;
      if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (button)))
	{
	  if (l->next)
	    {
	      GtkWidget *next_button = BUTTON_DATA (l->next->data)->button;
	      button_clicked_cb (next_button, l->next->data);
	    }
	  break;
	}
    }
}

/**
 * _gtk_path_bar_down:
 * @path_bar: a #GtkPathBar
 * 
 * If the selected button in the pathbar is not the furthest button “down” (in the
 * leaf direction), act as if the user clicked on the next button down.
 **/
void
_gtk_path_bar_down (GtkPathBar *path_bar)
{
  GList *l;

  for (l = path_bar->priv->button_list; l; l = l->next)
    {
      GtkWidget *button = BUTTON_DATA (l->data)->button;
      if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (button)))
	{
	  if (l->prev)
	    {
	      GtkWidget *prev_button = BUTTON_DATA (l->prev->data)->button;
	      button_clicked_cb (prev_button, l->prev->data);
	    }
	  break;
	}
    }
}
