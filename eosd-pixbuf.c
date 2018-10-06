#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <emacs-module.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

int plugin_is_GPL_compatible;


/* Thanks to http://phst.github.io/emacs-modules.html#copy_string_contents */
static bool
copy_string_contents (emacs_env *env, emacs_value value,
                      char **buffer, size_t *size)
{
  ptrdiff_t buffer_size;
  if (!env->copy_string_contents (env, value, NULL, &buffer_size))
    return false;
  assert (env->non_local_exit_check (env) == emacs_funcall_exit_return);
  assert (buffer_size > 0);
  *buffer = malloc ((size_t) buffer_size);
  if (*buffer == NULL) {
    env->non_local_exit_signal (env, env->intern (env, "memory-full"),
                                env->intern (env, "nil"));
    return false;
  }
  ptrdiff_t old_buffer_size = buffer_size;
  if (!env->copy_string_contents (env, value, *buffer, &buffer_size)) {
    free (*buffer);
    *buffer = NULL;
    return false;
  }
  assert (env->non_local_exit_check (env) == emacs_funcall_exit_return);
  assert (buffer_size == old_buffer_size);
  *size = (size_t) (buffer_size - 1);
  return true;
}

/* Always return symbol 't'.  */
static emacs_value
Feosd_pixbuf_to_png (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  GError *error = NULL;
  GdkPixbuf *pixbuf;
  gboolean alpha;
  int width, height, rowstride, bits_per_sample;
  size_t data_size = 0;
  gchar *buffer = NULL;
  gsize newsize;
  emacs_value thedata, output;

  /* Read parameter list */
  width = env->extract_integer (env, args[0]);
  height = env->extract_integer (env, args[1]);
  rowstride = env->extract_integer (env, args[2]);
  alpha = env->extract_integer (env, args[3]) == 1;
  bits_per_sample = env->extract_integer (env, args[4]);
  thedata = args[5];

  /* Copy contents of the data to a buffer */
  copy_string_contents (env, thedata, &buffer, &data_size);

  /* Get data into a pixbuf instance so we can convert to an image
     format Emacs understands. */
  pixbuf = gdk_pixbuf_new_from_data ((guchar *) buffer,
                                     GDK_COLORSPACE_RGB,
                                     alpha,
                                     bits_per_sample,
                                     width,
                                     height,
                                     rowstride,
                                     NULL, NULL); /* Memory manage stays with emacs */
  if (!gdk_pixbuf_save_to_buffer (pixbuf, &buffer, &newsize, "png", &error, NULL)) {
    g_object_unref (pixbuf);
    free (buffer);
    return env->intern (env, "nil");
  }
  output = env->make_string (env, buffer, data_size - 1);
  g_object_unref (pixbuf);
  free (buffer);
  return output;
}

/* Lisp utilities for easier readability (simple wrappers).  */

/* Provide FEATURE to Emacs.  */
static void
provide (emacs_env *env, const char *feature)
{
  emacs_value _Qfeat = env->intern (env, feature);
  emacs_value _Qprovide = env->intern (env, "provide");
  emacs_value args[] = { _Qfeat };

  env->funcall (env, _Qprovide, 1, args);
}

/* Bind NAME to FUN.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qfset, 2, args);
}

/* Module init function.  */
extern int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);

#define _DEFUN(lsym, csym, amin, amax, doc, data) \
  bind_function (env, lsym, \
		 env->make_function (env, amin, amax, csym, doc, data))

  _DEFUN ("eosd-pixbuf-to-png", Feosd_pixbuf_to_png, 6, 6, NULL, NULL);

  provide (env, "eosd-pixbuf");
  return 0;
}
