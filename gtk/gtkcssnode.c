/* GTK - The GIMP Toolkit
 * Copyright (C) 2014 Benjamin Otte <otte@gnome.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library. If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"

#include "gtkcssnodeprivate.h"

#include "gtkcsstransientnodeprivate.h"
#include "gtkdebug.h"
#include "gtksettingsprivate.h"

G_DEFINE_TYPE (GtkCssNode, gtk_css_node, G_TYPE_OBJECT)

void
gtk_css_node_set_invalid (GtkCssNode *node,
                          gboolean    invalid)
{
  if (node->invalid == invalid)
    return;

  GTK_CSS_NODE_GET_CLASS (node)->set_invalid (node, invalid);
}

static void
gtk_css_node_dispose (GObject *object)
{
  GtkCssNode *cssnode = GTK_CSS_NODE (object);

  while (cssnode->first_child)
    {
      gtk_css_node_set_parent (cssnode->first_child, NULL);
    }

  G_OBJECT_CLASS (gtk_css_node_parent_class)->dispose (object);
}

static void
gtk_css_node_finalize (GObject *object)
{
  GtkCssNode *cssnode = GTK_CSS_NODE (object);

  if (cssnode->style)
    g_object_unref (cssnode->style);
  gtk_css_node_declaration_unref (cssnode->decl);

  G_OBJECT_CLASS (gtk_css_node_parent_class)->finalize (object);
}

static gboolean
may_use_global_parent_cache (GtkCssNode *node)
{
  GtkCssNode *parent;
  
  parent = gtk_css_node_get_parent (node);
  if (parent == NULL)
    return FALSE;

  if (gtk_css_node_get_style_provider (node) != gtk_css_node_get_style_provider (parent))
    return FALSE;

  return TRUE;
}

static GtkCssStyle *
lookup_in_global_parent_cache (GtkCssNode                  *node,
                               GtkCssStyle                 *parent,
                               const GtkCssNodeDeclaration *decl)
{
  GHashTable *cache;
  GtkCssStyle *style;

  if (parent == NULL ||
      !may_use_global_parent_cache (node))
    return NULL;

  cache = g_object_get_data (G_OBJECT (parent), "gtk-global-cache");
  if (cache == NULL)
    return NULL;

  style = g_hash_table_lookup (cache, decl);

  return style;
}

static gboolean
may_be_stored_in_parent_cache (GtkCssStyle *style)
{
  GtkCssChange change;

  change = gtk_css_static_style_get_change (GTK_CSS_STATIC_STYLE (style));

  /* The cache is shared between all children of the parent, so if a
   * style depends on a sibling it is not independant of the child.
   */
  if (change & GTK_CSS_CHANGE_ANY_SIBLING)
    return FALSE;

  /* Again, the cache is shared between all children of the parent.
   * If the position is relevant, no child has the same style.
   */
  if (change & GTK_CSS_CHANGE_POSITION)
    return FALSE;

  return TRUE;
}

static void
store_in_global_parent_cache (GtkCssNode                  *node,
                              GtkCssStyle                 *parent,
                              const GtkCssNodeDeclaration *decl,
                              GtkCssStyle                 *style)
{
  GHashTable *cache;

  g_assert (GTK_IS_CSS_STATIC_STYLE (style));

  if (parent == NULL ||
      !may_use_global_parent_cache (node))
    return;

  if (!may_be_stored_in_parent_cache (style))
    return;

  cache = g_object_get_data (G_OBJECT (parent), "gtk-global-cache");
  if (cache == NULL)
    {
      cache = g_hash_table_new_full (gtk_css_node_declaration_hash,
                                     gtk_css_node_declaration_equal,
                                     (GDestroyNotify) gtk_css_node_declaration_unref,
                                     g_object_unref);
      g_object_set_data_full (G_OBJECT (parent), "gtk-global-cache", cache, (GDestroyNotify) g_hash_table_destroy);
    }

  g_hash_table_insert (cache,
                       gtk_css_node_declaration_ref ((GtkCssNodeDeclaration *) decl),
                       g_object_ref (style));
}

GtkCssStyle *
gtk_css_node_update_style (GtkCssNode       *cssnode,
                           GtkCssStyle      *style,
                           const GtkBitmask *parent_changes)
{
  const GtkCssNodeDeclaration *decl;
  GtkCssMatcher matcher;
  GtkWidgetPath *path;
  GtkCssStyle *parent;
  GtkCssStyle *result;

  parent = cssnode->parent ? cssnode->parent->style : NULL;
  decl = gtk_css_node_get_declaration (cssnode);

  result = lookup_in_global_parent_cache (cssnode, parent, decl);
  if (result)
    return g_object_ref (result);

  path = gtk_css_node_create_widget_path (cssnode);

  if (!_gtk_css_matcher_init (&matcher, path))
    {
      g_assert_not_reached ();
    }

  result = gtk_css_static_style_new_update (GTK_CSS_STATIC_STYLE (style),
                                            parent_changes,
                                            gtk_css_node_get_style_provider (cssnode),
                                            &matcher,
                                            parent);

  gtk_widget_path_free (path);

  store_in_global_parent_cache (cssnode, parent, decl, style);

  return result;
}

GtkCssStyle *
gtk_css_node_create_style (GtkCssNode *cssnode)
{
  const GtkCssNodeDeclaration *decl;
  GtkCssMatcher matcher;
  GtkWidgetPath *path;
  GtkCssStyle *parent;
  GtkCssStyle *style;

  decl = gtk_css_node_get_declaration (cssnode);
  parent = cssnode->parent ? cssnode->parent->style : NULL;

  style = lookup_in_global_parent_cache (cssnode, parent, decl);
  if (style)
    return g_object_ref (style);

  path = gtk_css_node_create_widget_path (cssnode);

  if (_gtk_css_matcher_init (&matcher, path))
    style = gtk_css_static_style_new_compute (gtk_css_node_get_style_provider (cssnode),
                                              &matcher,
                                              parent);
  else
    style = gtk_css_static_style_new_compute (gtk_css_node_get_style_provider (cssnode),
                                              NULL,
                                              parent);

  gtk_widget_path_free (path);

  store_in_global_parent_cache (cssnode, parent, decl, style);

  return style;
}

static void
gtk_css_node_real_invalidate (GtkCssNode   *cssnode,
                              GtkCssChange  change)
{
}

static void
gtk_css_node_real_set_invalid (GtkCssNode *node,
                               gboolean    invalid)
{
  node->invalid = invalid;

  if (invalid && node->parent)
    gtk_css_node_set_invalid (node->parent, invalid);
}

static GtkBitmask *
gtk_css_node_real_validate (GtkCssNode       *cssnode,
                            gint64            timestamp,
                            GtkCssChange      change,
                            const GtkBitmask *parent_changes)
{
  return _gtk_bitmask_new ();
}

static GtkWidgetPath *
gtk_css_node_real_create_widget_path (GtkCssNode *cssnode)
{
  return gtk_widget_path_new ();
}

static const GtkWidgetPath *
gtk_css_node_real_get_widget_path (GtkCssNode *cssnode)
{
  return NULL;
}

static GtkStyleProviderPrivate *
gtk_css_node_real_get_style_provider (GtkCssNode *cssnode)
{
  if (cssnode->parent)
    return gtk_css_node_get_style_provider (cssnode->parent);

  return GTK_STYLE_PROVIDER_PRIVATE (_gtk_settings_get_style_cascade (gtk_settings_get_default (), 1));
}

static void
gtk_css_node_class_init (GtkCssNodeClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = gtk_css_node_dispose;
  object_class->finalize = gtk_css_node_finalize;

  klass->invalidate = gtk_css_node_real_invalidate;
  klass->validate = gtk_css_node_real_validate;
  klass->set_invalid = gtk_css_node_real_set_invalid;
  klass->create_widget_path = gtk_css_node_real_create_widget_path;
  klass->get_widget_path = gtk_css_node_real_get_widget_path;
  klass->get_style_provider = gtk_css_node_real_get_style_provider;
}

static void
gtk_css_node_init (GtkCssNode *cssnode)
{
  cssnode->decl = gtk_css_node_declaration_new ();
}

void
gtk_css_node_set_parent (GtkCssNode *node,
                         GtkCssNode *parent)
{
  if (node->parent == parent)
    return;

  /* Take a reference here so the whole function has a reference */
  g_object_ref (node);

  if (node->parent != NULL)
    {
      if (!GTK_IS_CSS_TRANSIENT_NODE (node))
        {
          if (node->previous_sibling)
            node->previous_sibling->next_sibling = node->next_sibling;
          else
            node->parent->first_child = node->next_sibling;

          if (node->next_sibling)
            node->next_sibling->previous_sibling = node->previous_sibling;
          else
            node->parent->last_child = node->previous_sibling;

          node->parent->n_children--;
        }

      node->parent = NULL;
      node->next_sibling = NULL;
      node->previous_sibling = NULL;

      g_object_unref (node);
    }

  if (parent)
    {
      node->parent = parent;

      if (!GTK_IS_CSS_TRANSIENT_NODE (node))
        {
          parent->n_children++;

          if (parent->last_child)
            {
              parent->last_child->next_sibling = node;
              node->previous_sibling = parent->last_child;
            }
          parent->last_child = node;

          if (parent->first_child == NULL)
            parent->first_child = node;
        }

      if (node->invalid)
        gtk_css_node_set_invalid (parent, TRUE);
    }

  gtk_css_node_invalidate (node, GTK_CSS_CHANGE_ANY_PARENT | GTK_CSS_CHANGE_ANY_SIBLING);

  if (node->parent == NULL)
    g_object_unref (node);
}

GtkCssNode *
gtk_css_node_get_parent (GtkCssNode *cssnode)
{
  return cssnode->parent;
}

GtkCssNode *
gtk_css_node_get_first_child (GtkCssNode *cssnode)
{
  return cssnode->first_child;
}

GtkCssNode *
gtk_css_node_get_last_child (GtkCssNode *cssnode)
{
  return cssnode->last_child;
}

GtkCssNode *
gtk_css_node_get_previous_sibling (GtkCssNode *cssnode)
{
  return cssnode->previous_sibling;
}

GtkCssNode *
gtk_css_node_get_next_sibling (GtkCssNode *cssnode)
{
  return cssnode->next_sibling;
}

void
gtk_css_node_set_style (GtkCssNode  *cssnode,
                        GtkCssStyle *style)
{
  if (cssnode->style == style)
    return;

  if (style)
    g_object_ref (style);

  if (cssnode->style)
    g_object_unref (cssnode->style);

  cssnode->style = style;
}

GtkCssStyle *
gtk_css_node_get_style (GtkCssNode *cssnode)
{
  return cssnode->style;
}

void
gtk_css_node_set_widget_type (GtkCssNode *cssnode,
                              GType       widget_type)
{
  if (gtk_css_node_declaration_set_type (&cssnode->decl, widget_type))
    gtk_css_node_invalidate (cssnode, GTK_CSS_CHANGE_NAME);
}

GType
gtk_css_node_get_widget_type (GtkCssNode *cssnode)
{
  return gtk_css_node_declaration_get_type (cssnode->decl);
}

void
gtk_css_node_set_id (GtkCssNode *cssnode,
                     const char *id)
{
  if (gtk_css_node_declaration_set_id (&cssnode->decl, id))
    gtk_css_node_invalidate (cssnode, GTK_CSS_CHANGE_ID);
}

const char *
gtk_css_node_get_id (GtkCssNode *cssnode)
{
  return gtk_css_node_declaration_get_id (cssnode->decl);
}

void
gtk_css_node_set_state (GtkCssNode    *cssnode,
                        GtkStateFlags  state_flags)
{
  if (gtk_css_node_declaration_set_state (&cssnode->decl, state_flags))
    gtk_css_node_invalidate (cssnode, GTK_CSS_CHANGE_STATE);
}

GtkStateFlags
gtk_css_node_get_state (GtkCssNode *cssnode)
{
  return gtk_css_node_declaration_get_state (cssnode->decl);
}

void
gtk_css_node_set_junction_sides (GtkCssNode       *cssnode,
                                 GtkJunctionSides  junction_sides)
{
  gtk_css_node_declaration_set_junction_sides (&cssnode->decl, junction_sides);
}

GtkJunctionSides
gtk_css_node_get_junction_sides (GtkCssNode *cssnode)
{
  return gtk_css_node_declaration_get_junction_sides (cssnode->decl);
}

void
gtk_css_node_add_class (GtkCssNode *cssnode,
                        GQuark      style_class)
{
  if (gtk_css_node_declaration_add_class (&cssnode->decl, style_class))
    gtk_css_node_invalidate (cssnode, GTK_CSS_CHANGE_CLASS);
}

void
gtk_css_node_remove_class (GtkCssNode *cssnode,
                           GQuark      style_class)
{
  if (gtk_css_node_declaration_remove_class (&cssnode->decl, style_class))
    gtk_css_node_invalidate (cssnode, GTK_CSS_CHANGE_CLASS);
}

gboolean
gtk_css_node_has_class (GtkCssNode *cssnode,
                        GQuark      style_class)
{
  return gtk_css_node_declaration_has_class (cssnode->decl, style_class);
}

GList *
gtk_css_node_list_classes (GtkCssNode *cssnode)
{
  return gtk_css_node_declaration_list_classes (cssnode->decl);
}

void
gtk_css_node_add_region (GtkCssNode     *cssnode,
                         GQuark          region,
                         GtkRegionFlags  flags)
{
  if (gtk_css_node_declaration_add_region (&cssnode->decl, region, flags))
    gtk_css_node_invalidate (cssnode, GTK_CSS_CHANGE_REGION);
}

void
gtk_css_node_remove_region (GtkCssNode *cssnode,
                            GQuark      region)
{
  if (gtk_css_node_declaration_remove_region (&cssnode->decl, region))
    gtk_css_node_invalidate (cssnode, GTK_CSS_CHANGE_REGION);
}

gboolean
gtk_css_node_has_region (GtkCssNode     *cssnode,
                         GQuark          region,
                         GtkRegionFlags *out_flags)
{
  return gtk_css_node_declaration_has_region (cssnode->decl, region, out_flags);
}

GList *
gtk_css_node_list_regions (GtkCssNode *cssnode)
{
  return gtk_css_node_declaration_list_regions (cssnode->decl);
}


const GtkCssNodeDeclaration *
gtk_css_node_get_declaration (GtkCssNode *cssnode)
{
  return cssnode->decl;
}

void
gtk_css_node_invalidate (GtkCssNode   *cssnode,
                         GtkCssChange  change)
{
  cssnode->pending_changes |= change;

  GTK_CSS_NODE_GET_CLASS (cssnode)->invalidate (cssnode, change);

  gtk_css_node_set_invalid (cssnode, TRUE);
}

void
gtk_css_node_validate (GtkCssNode            *cssnode,
                       gint64                 timestamp,
                       GtkCssChange           change,
                       const GtkBitmask      *parent_changes)
{
  GtkCssNode *child;
  GtkBitmask *changes;

  /* If you run your application with
   *   GTK_DEBUG=no-css-cache
   * every invalidation will purge the cache and completely query
   * everything anew form the cache. This is slow (in particular
   * when animating), but useful for figuring out bugs.
   *
   * We achieve that by pretending that everything that could have
   * changed has and so we of course totally need to redo everything.
   *
   * Note that this also completely revalidates child widgets all
   * the time.
   */
  if (G_UNLIKELY (gtk_get_debug_flags () & GTK_DEBUG_NO_CSS_CACHE))
    change = GTK_CSS_CHANGE_ANY;

  if (!cssnode->invalid && change == 0 && _gtk_bitmask_is_empty (parent_changes))
    return;

  gtk_css_node_set_invalid (cssnode, FALSE);

  change |= cssnode->pending_changes;
  cssnode->pending_changes = 0;

  changes = GTK_CSS_NODE_GET_CLASS (cssnode)->validate (cssnode, timestamp, change, parent_changes);

  change = _gtk_css_change_for_child (change);

  for (child = gtk_css_node_get_first_child (cssnode);
       child;
       child = gtk_css_node_get_next_sibling (child))
    {
      gtk_css_node_validate (child, timestamp, change, changes);
    }

  _gtk_bitmask_free (changes);
}

GtkWidgetPath *
gtk_css_node_create_widget_path (GtkCssNode *cssnode)
{
  return GTK_CSS_NODE_GET_CLASS (cssnode)->create_widget_path (cssnode);
}

const GtkWidgetPath *
gtk_css_node_get_widget_path (GtkCssNode *cssnode)
{
  return GTK_CSS_NODE_GET_CLASS (cssnode)->get_widget_path (cssnode);
}

GtkStyleProviderPrivate *
gtk_css_node_get_style_provider (GtkCssNode *cssnode)
{
  return GTK_CSS_NODE_GET_CLASS (cssnode)->get_style_provider (cssnode);
}
