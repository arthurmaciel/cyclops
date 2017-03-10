;; TODO: create a cyclone library for command line arguments??
;; TODO: split and join should be in core as well, in some form
;;       generalize them to functions on lists, then provide string-splilt, string-join

;; Roadmap:
;- modularize code, add a command line interface

; changes to server "cyclone-packages":
; - on the packages side, could maintain a package index that might just be a zipped list of all the package.scm files on the server
; - to sync up, a cyclops client could just download the latest index file.
; - need a script to build up an index file, and possibly to create packages (basically just tar/gzip each package directory)

;- add a concept of a local repo "db" to keep track of:
;  - installed packages
;    - their versions
;    - their package.scm file (possibly)
;  - latest synced "index" file from the server

;- add a distributed layer/tool to work with remote repos
;  - could use libcurl to download files: https://curl.haxx.se/libcurl/c/url2file.html

;- add concept of upgrading using version info
;- allow packing/unpacking a package
;- Add a dependency resolver to pull in deps when a package is installed/updated/removed

(import 
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme file)
  (util)
)

;; TODO: constants to later externalize
(define *dest-dir* "/home/justin/Documents/cyclops") ;; TODO: temporarily use local for testing
(define *lib-dir* "/usr/local/share/cyclone")

;; run-directive :: string -> alist -> symbol -> void
;; Lookup given key in the alist of package parameters, 
;; and execute its directive if found.
(define (run-directive pkg-file-dir params key)
  (let ((directive (assoc key params)))
    (cond
     (directive
      ;; Set the appropriate directory first
      (with-chdir pkg-file-dir (lambda ()
        (for-each
          (lambda (cmd)
            (run-sys-cmd 
              (string-replace-all
                (string-replace-all cmd "~DESTDIR~" *dest-dir*)
                "~LIB~" *lib-dir*)))
          (cdr directive))))))))

;; Read given file and determine if it is a library
(define (file-is-library? filename)
  (call-with-input-file filename (lambda (fp)
    (let ((contents (read fp)))
      (library? contents)))))

;; Read all of the parameters from the given package file port
(define (read-pkg fp cmd pkg-file-dir)
  (let ((params (read-all fp)))
    (cond
      ((equal? cmd "install")
       (run-directive pkg-file-dir params 'build)
       (run-directive pkg-file-dir params 'install))
      ((equal? cmd "uninstall")
       (run-directive pkg-file-dir params 'uninstall))
      ((equal? cmd "test")
       (run-directive pkg-file-dir params 'test))
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

;; Main 
(let* ((args (command-line-arguments))
      )
  (cond
    ((or (null? args)
         (< (length args) 2))
     (write "Usage: cyclone command package-file")
     (newline)
     (exit 1))
    (else
      (let ((cmd (car args))
            (pkgfile (cadr args)))
        (call-with-input-file
          pkgfile
          (lambda (fp)
            (read-pkg fp cmd (filename->path pkgfile))))))))
