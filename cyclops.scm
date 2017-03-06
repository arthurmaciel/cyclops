;; Roadmap:
;- get install, test, uninstall working locally
;- consider, may be more flexible to have a build() target that puts everything in a sub-directory and overlays that to install, vs separate file/install/uninstall sections. look a PKGBUILD for an example: https://github.com/justinethier/cyclone-bootstrap/blob/master/dist/PKGBUILD
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
(define *cyclone-repo-bin-dir* "./bin") ;; TODO: temporarily use local for testing
(define *cyclone-repo-lib-dir* ".") ;; TODO: temporarily use local for testing
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
          run-sys-cmd
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
      ((eq? cmd 'install)
       (install params))
      ((eq? cmd 'uninstall)
       (uninstall params))
      ((eq? cmd 'test)
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

;; install :: alist -> void
;; Build & Install files using the given package file parameters
(define (install params)
  (define install-file-list '())
  ;; Build files
  (for-each
    (lambda (file)
      (let* ((file/path (string-append *pkg-file-dir* "/" file))
             (lib? (file-is-library? file/path))
             (file/path-no-ext (basename file/path)))
(write `(,file/path ,(file-is-library? file/path)))(newline) ;; DEBUG
        (run-sys-cmd
          "cyclone " 
          file/path)
      ;; TODO: check return code, make sure build succeeded
      ;; TODO: if library, install .o .sld .meta files
      (cond
        (lib?
          (set! install-file-list 
             (append 
               install-file-list 
               (map
                 (lambda (file)
                   (cons *cyclone-repo-lib-dir* file))
                 (list 
                  file/path
                  (string-append file/path-no-ext ".o")
                  (string-append file/path-no-ext ".meta")
                 ))))) ;; TODO: possibly any included .scm files, too
        (else
          (set! install-file-list 
             (append 
               install-file-list 
               (list 
                (cons *cyclone-repo-bin-dir* file/path-no-ext) ;; compiled executable
               )))))
    ))
    (get-file-list params))

  ;; Install files
  ;(write `(files to install: ,install-file-list)) (newline) ;; more DEBUGGING
  (if (not (file-exists? *cyclone-repo-bin-dir*))
      (run-sys-cmd "mkdir " *cyclone-repo-bin-dir*))
  (if (not (file-exists? *cyclone-repo-lib-dir*))
      (run-sys-cmd "mkdir " *cyclone-repo-lib-dir*))
  (for-each
    (lambda (dest/filename)
      ; Strip off leading directory
      (let* ((dest (car dest/filename))
             (filename (cdr dest/filename))
             (pkg-basedir-len (string-length *pkg-file-dir*))
             (fn-no-base (substring filename pkg-basedir-len (string-length filename))))
        (create-missing-dirs fn-no-base dest)
        (run-sys-cmd "cp " filename " " dest "/" fn-no-base)
      ))
    install-file-list)

  ;; Run the 'install' section, if applicable 
  (run-directive params 'install)
  )

(define (uninstall params)
  (define file-list '())
  ;; Build up the list of installed files
  (for-each
    (lambda (file)
      (let* ((file/path (string-append *pkg-file-dir* "/" file))
             (lib? (file-is-library? file/path))
             (file/path-no-ext (basename file/path)))
    (write `(,file/path ,(file-is-library? file/path)))(newline) ;; DEBUG
      (cond
        (lib?
          (set! file-list 
             (append 
               file-list 
               (map
                 (lambda (file)
                   (cons *cyclone-repo-lib-dir* file))
                 (list 
                  file/path
                  (string-append file/path-no-ext ".o")
                  (string-append file/path-no-ext ".meta")
                 ))))) ;; TODO: possibly any included .scm files, too
        (else
          (set! file-list 
             (append 
               file-list 
               (list 
                (cons *cyclone-repo-bin-dir* file/path-no-ext) ;; compiled executable
               )))))))
    (get-file-list params))
(write `(uninstall list ,file-list))(newline)
;  ;; Uninstall each file in file-list
  (for-each
    (lambda (dest/filename)
      ;; TODO: delete the installed files, leave dirs (at least for now)
      (write `(TODO uninstall ,dest/filename))(newline)
    )
    file-list)

  ;; Run the 'uninstall' section, if applicable 
  (run-directive params 'uninstall)
)

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

;(write (chdir *pkg-file-dir*))
;(write (getcwd))
(call-with-input-file 
  *pkg-file*
  (lambda (fp)
    ;(read-pkg fp 'install)
    (read-pkg fp 'uninstall)
    ;(read-pkg fp 'test)
  ))

#;(write
  (split "/scheme/cyclone/sample.sld" #\/))

#;(write
  (create-missing-dirs "scheme/cyclone/sample.sld" "."))
