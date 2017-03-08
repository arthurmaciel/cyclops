;; Roadmap:
;- get install, test, uninstall working locally
;- modularize code, add a command line interface
;- add a concept of a local repo "db" to keep track of:
;  - installed packages
;  - their versions
;  - their package.scm file
;- add concept of upgrading using version info
;- allow packing/unpacking a package
;- add a distributed layer/tool to work with remote repos
;- Add a dependency resolver to pull in deps when a package is installed/updated/removed

(import 
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme file)
  (scheme cyclone libraries)
  (scheme cyclone transforms)
  (scheme cyclone util)
)
(include-c-header "<unistd.h>")

;; TODO: constants to later externalize
(define *dest-dir* "/home/justin/Documents/cyclops") ;; TODO: temporarily use local for testing
(define *lib-dir* "/usr/local/share/cyclone")
(define *pkg-file-dir* "../cyclone-packages/sample-lib")
(define *pkg-file* "../cyclone-packages/sample-lib/package.scm")

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

(define-c getcwd
  "(void *data, int argc, closure _, object k)"
  " char cwd[PATH_MAX];
    if (getcwd(cwd, sizeof(cwd)) == NULL) {
      Cyc_rt_raise_msg(data, \"Error calling get-cwd\");
    } else {
      make_string(str, cwd);
      return_closcall1(data, k, &str);
    }")

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

;; run-directive :: alist -> symbol -> void
;; Lookup given key in the alist of package parameters, 
;; and execute its directive if found.
(define (run-directive params key)
  (let ((directive (assoc key params)))
    (cond
     (directive
      ;; Set the appropriate directory first
      (with-chdir *pkg-file-dir* (lambda ()
        (for-each
          (lambda (cmd)
            (run-sys-cmd 
              (string-replace-all
                (string-replace-all cmd "~DESTDIR~" *dest-dir*)
                "~LIB~" *lib-dir*)))
          (cdr directive))))))))

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

;; Read given file and determine if it is a library
(define (file-is-library? filename)
  (call-with-input-file filename (lambda (fp)
    (let ((contents (read fp)))
      (library? contents)))))

;; Read all of the parameters from the given package file port
(define (read-pkg fp cmd)
  (let ((params (read-all fp)))
    (cond
      ((equal? cmd "install")
       (run-directive params 'build)
       (run-directive params 'install))
      ((equal? cmd "uninstall")
       (run-directive params 'uninstall))
      ((equal? cmd "test")
       (run-directive params cmd))
      (else
       (error "Unsupported command" cmd)))))

;; Extract file list from the given package file parameters
(define (get-file-list params)
  (let ((files (assoc 'files params)))
    (if (not files)
        '()
        (map
          (lambda (filename)
            (cond
              ((string? filename)
               filename)
              ((pair? filename)
               (lib:import->filename filename))
              (else
                (error "Unsupported filename" filename))))
          (cdr files)))))

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

(let* ((args (command-line-arguments))
      )
  (cond
    ((or (null? args)
         (< (length args) 2))
     (write "TODO: usage")
     (newline)
     (exit 1))
    (else
      (let ((cmd (car args))
            (pkgfile (cadr args)))
        (write `(pkg dir ,(basename pkgfile)))
        (call-with-input-file
          pkgfile
          (lambda (fp)
            (read-pkg fp cmd))))
      )))
;; TODO: create a cyclone library for command line arguments??


;(write (chdir *pkg-file-dir*))
;(write (getcwd))
;(call-with-input-file 
;  *pkg-file*
;  (lambda (fp)
;    ;(read-pkg fp 'install)
;    (read-pkg fp 'uninstall)
;    ;(read-pkg fp 'test)
;  ))
;
;#;(write
;  (split "/scheme/cyclone/sample.sld" #\/))
;
;#;(write
;  (create-missing-dirs "scheme/cyclone/sample.sld" "."))
