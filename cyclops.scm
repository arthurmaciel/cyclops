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
(define *cyclops-db:dir* "./repo")
(define *remote-repo-url* "https://raw.githubusercontent.com/cyclone-scheme/cyclone-packages/master/_packages/")
(define *tmp-dir* "/tmp")

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
             *cyclops-db:dir*
             "/"
             (pkg-file-dir->pkg-name pkg-file-dir))))
    ;(display `(debug ,pkg-file-dir ,params))
    (cond
      ((equal? cmd "install-local")
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
      ((equal? cmd "uninstall-local")
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

(define (usage)
  (display "Usage: cyclone command package-file")
  (newline))

(define (local-db:get name)
  (let ((index-file (string-append *cyclops-db:dir* "/index.dat")))
    (cond
      ((not (file-exists? index-file))
       #f)
      (else
        (call-with-input-file
          index-file
          (lambda (fp)
            (let ((entry (assoc (string->symbol name) (read-all fp))))
              `((name . ,(car entry))
                (ver  . ,(cadr entry))
                (file . ,(caddr entry))))))))))

;; Main 
(let* ((args (command-line-arguments))
       (cmd (if (null? args) #f (car args))))
  (cond
    ((null? args)
     (usage))
    ((equal? cmd "sync")
     (display "Downloading remote repository index...")
     (newline)
     (run-sys-cmd
       "mkdir -p " *cyclops-db:dir*)
     (download 
       (string-append *remote-repo-url* "index.dat")
       (string-append *cyclops-db:dir* "/index.dat")))
;; TODO: need a query command to query for installed packages
;; TODO: following need to work with the package name rather than the
;; package file.
;; - for install, need to download the remote file (though may want to check
;;   local package.scm and version from index.dat), then when
;;   that file is available do the current install using the download.
;;   once everything is done need to save the package.scm in the appropriate
;;   place and make any other needed local DB entries
;; - function is: uninstall/test, look up downloaded package file (?) and exec
;;   commands if necessary. probably only want to save the package.scm file
;;   though, which may change how tests are run. or maybe the test dir is
;;   also saved
    ((equal? cmd "install")
     ;; TODO: check if a sync is needed (IE, there is no local DB)
     (let* ((package-name (cadr args))
            (package-info (local-db:get package-name)))
       (cond
         ((not package-info)
          ;; TODO: how to handle this case?
          (display "Package not found!"))
         (else
           (let* ((fname (symbol->string
                           (cdr
                            (assoc 'file package-info))))
                  (tmp-file (string-append *tmp-dir* "/" fname)))
             (download
               (string-append *remote-repo-url* fname)
               tmp-file)
             (run-sys-cmd "cd " *tmp-dir* "; tar xfz " tmp-file)
             ;(write `(TODO install-local ,(filename->path tmp-file))))
             (process-pkg "install-local" (string-append *tmp-dir* "/" package-name "/package.scm")))))))
    ((equal? cmd "uninstall")
     (let* ((package-name (cadr args))
            (package-info (local-db:get package-name)))
       (process-pkg 
         "uninstall-local" 
         (string-append 
           *cyclops-db:dir*
           "/" package-name "/package.scm")))
     ;; TODO: look up package in local repo, call uninstall section from package.scm (probably via uninstall-local)
     ;; TODO: probably do something similar for test
     ;(write 'TODO)
    )
    ((member cmd '("install-local" "uninstall-local" "test"))
      (let ((cmd (car args))
            (pkgfile (cadr args)))
        (process-pkg cmd pkgfile)))
    (else
     (usage))))
