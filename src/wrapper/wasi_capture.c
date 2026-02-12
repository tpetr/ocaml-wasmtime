/* C-level capture buffer for wasi_config_set_stdout_custom/stderr_custom.
   All buffering happens in C, avoiding OCaml callback/GC/domain-lock issues. */

#include <wasi.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>

typedef struct {
    unsigned char *data;
    size_t len;
    size_t cap;
} wasi_capture_buf_t;

wasi_capture_buf_t *wasi_capture_buf_new(void) {
    return calloc(1, sizeof(wasi_capture_buf_t));
}

static ptrdiff_t wasi_capture_buf_write(void *ptr, const unsigned char *bytes, size_t len) {
    wasi_capture_buf_t *buf = (wasi_capture_buf_t *)ptr;
    if (buf->len + len > buf->cap) {
        size_t new_cap = buf->cap == 0 ? 256 : buf->cap;
        while (new_cap < buf->len + len) new_cap *= 2;
        unsigned char *new_data = realloc(buf->data, new_cap);
        if (!new_data) return -1;
        buf->data = new_data;
        buf->cap = new_cap;
    }
    memcpy(buf->data + buf->len, bytes, len);
    buf->len += len;
    return (ptrdiff_t)len;
}

static void wasi_capture_buf_noop_finalizer(void *ptr) {
    /* Don't free here - OCaml side reads contents then calls _free */
    (void)ptr;
}

void wasi_capture_set_stdout(wasi_config_t *config, wasi_capture_buf_t *buf) {
    wasi_config_set_stdout_custom(config, wasi_capture_buf_write, buf, wasi_capture_buf_noop_finalizer);
}

void wasi_capture_set_stderr(wasi_config_t *config, wasi_capture_buf_t *buf) {
    wasi_config_set_stderr_custom(config, wasi_capture_buf_write, buf, wasi_capture_buf_noop_finalizer);
}

unsigned char *wasi_capture_buf_data(wasi_capture_buf_t *buf) {
    return buf->data ? buf->data : (unsigned char *)"";
}

size_t wasi_capture_buf_len(wasi_capture_buf_t *buf) {
    return buf->len;
}

void wasi_capture_buf_free(wasi_capture_buf_t *buf) {
    if (buf) {
        free(buf->data);
        free(buf);
    }
}
