;; Roadmap:

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
  (scheme cyclone util)
  (download)
  (util)
)

;; TODO: constants to later externalize
(define *dest-dir* "/home/justin/Documents/cyclops") ;; TODO: temporarily use local for testing
(define *lib-dir* "/usr/local/share/cyclone")
(define *cyclops-db:installed-dir* "./installed")
(define *cyclops-db:repo-sync-dir* "./remote")
(define *remote-repo-url* "https://raw.githubusercontent.com/cyclone-scheme/cyclone-packages/master/_packages/")

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

(define (pkg-file-dir->pkg-name pkg-file-dir)
  (car (reverse (split pkg-file-dir #\/))))

;; Read all of the parameters from the given package file port
(define (process-pkg cmd pkgfile)
  (let* ((params (call-with-input-file
                   pkgfile
                   (lambda (fp) 
                     (read-all fp))))
         (pkg-file-dir
           (filename->path pkgfile))
         (cp:pkg-file-dir
           (string-append
             *cyclops-db:installed-dir*
             "/"
             (pkg-file-dir->pkg-name pkg-file-dir))))
    (cond
      ((equal? cmd "install")
       (run-directive pkg-file-dir params 'build)
       (run-directive pkg-file-dir params 'install)
       ;; Record installed package in local DB
       ;; This format is not good enough, but rather just a start
       (run-sys-cmd 
         "mkdir -p "
         cp:pkg-file-dir)
       (run-sys-cmd
         "cp "
         pkgfile
         " "
         cp:pkg-file-dir
         "/"
         (filename->file pkgfile))
      )
      ((equal? cmd "uninstall")
       (run-directive pkg-file-dir params 'uninstall))
      ((equal? cmd "test")
       (run-directive pkg-file-dir params 'test))
      ((equal? cmd "sync")
       (run-sys-cmd
         "mkdir -p " *cyclops-db:repo-sync-dir*)
       (download 
         (string-append *remote-repo-url* "index.dat")
         (string-append *cyclops-db:repo-sync-dir* "/index.dat"))
      )
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
        (process-pkg cmd pkgfile)))))
