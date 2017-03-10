
(define-library (util)
  (import 
    (scheme cyclone util)
    (scheme base)
    (scheme write))
  (include-c-header "<unistd.h>")
  (export
    split
    join 
    filename->path
    run-sys-cmd
    create-missing-dirs
    with-chdir
    getcwd
    chdir)
  (begin

;; split :: string -> char -> [string]
;; Take the given string and split it into substrings separated by delim.
;; Delim must be a character and is not included in the substrings.
(define (split str delim)
  (filter 
    (lambda (s)
      (> (string-length s) 0))
    (reverse
      (map
        (lambda (lis)
          (list->string (reverse lis)))
        (foldl 
          (lambda (chr accum)
            (if (eq? chr delim)
              (cons '() accum)
              (cons 
                (cons chr (car accum))
                (cdr accum)))
          )
          '(())
          (string->list str))))))

;; join :: [string] -> char -> string
(define (join lis delim)
  (let ((delim-str (list->string (list delim))))
    (foldl 
      (lambda (str accum)
        (string-append 
          accum 
          str
          delim-str))
      ""
      lis)))

;; filename->path :: string -> string
;; Take the given filename and take just the "path" portion.
;; EG: "./pkg/sample/package.scm" => "./pkg/sample/"
(define (filename->path filename)
   (let* ((sub-dirs (split filename #\/))
          ;; Discard filename from full filename and path
          (path (if (null? sub-dirs)
                    sub-dirs
                    (reverse
                      (cdr
                        (reverse sub-dirs))))))
    (join path #\/)))

;; with-chdir :: string -> function -> void
;; Convenience function to temporarily change the current working dir
(define (with-chdir path thunk)
  (let ((cwd (getcwd)))
    (chdir path)
    (with-handler 
      (lambda (err)
        (write `(An error occurred ,err)))
      (thunk))
    (chdir cwd)))

;; Get current working directory
(define-c getcwd
  "(void *data, int argc, closure _, object k)"
  " char cwd[PATH_MAX];
    if (getcwd(cwd, sizeof(cwd)) == NULL) {
      Cyc_rt_raise_msg(data, \"Error calling get-cwd\");
    } else {
      make_string(str, cwd);
      return_closcall1(data, k, &str);
    }")

;; Change current working directory
(define-c chdir
  "(void *data, int argc, closure _, object k, object path)"
  " Cyc_check_str(data, path);
    if (chdir(string_str(path)) < 0) {
      Cyc_rt_raise_msg(data, \"Unable to change working directory\");
    } else {
      return_closcall1(data, k, obj_int2obj(0));
    }")

;; run-sys-cmd :: [string] -> integer
;; Concatenate given strings and run result as a system command
(define (run-sys-cmd . strs)
  (system
    (apply string-append strs)))

;; create-missing-dirs :: string -> string -> void
;; Accept file with a path destination, and create any missing directories
;; in the destination location.
(define (create-missing-dirs file/path dest-path)
  (let* (
         (dir (string-copy dest-path))
         (sub-dirs (split file/path #\/))
         ;; Discard filename from full filename and path
         (path (if (null? sub-dirs)
                   sub-dirs
                   (reverse
                     (cdr
                       (reverse sub-dirs)))))
        )
     (for-each
       (lambda (sub)
        (set! dir (string-append dir "/" sub))
        (if (not (file-exists? dir))
            (run-sys-cmd "mkdir " dir)))
       path)))

  ))
