;; Roadmap:
; 
; - Upgrade is just like install but with a version check prior. And possibly an uninstall as well (prior to install???)
; - Will need to update cyclone to recognize (cyclone) libs as coming from the data directory
; - Need to move all of the cyclops globals to a config file. Allow specifying config file location somehow as well, to better support sandboxes
; - Consider adding a define-c to get a temporary file name instead of using /tmp

(import 
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme file)
  (scheme cyclone util)
  (srfi 2)
  (download)
  (util)
)

;; TODO: constants to later externalize
(define *tmp-dir* "/tmp")
(define *prefix-dir* "/home/justin/Documents/cyclops") ;; TODO: temporarily use local for testing
(define *local-repo-dir* "./repo")
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
              (foldl 
                (lambda (old-new acc) 
                  (string-replace-all acc (car old-new) (cdr old-new))) 
                  cmd
                  `(("~BIN~" . ,(string-append *prefix-dir* "/bin"))
                    ("~LIB~" . ,(string-append *prefix-dir* "/lib"))
                    ("~INCLUDE~" . ,(string-append *prefix-dir* "/include/cyclone"))
                    ("~DATA~" .    ,(string-append *prefix-dir* "/share/cyclone"))
                   ))))
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
             *local-repo-dir*
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
       (run-directive pkg-file-dir params 'uninstall)
       ;; Remove package from local DB
       (run-sys-cmd
         "rm -rf " cp:pkg-file-dir)
      )
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
  (display "Usage: cyclops install package
       cyclops query package
       cyclops sync

cyclops is a simple package manager that supports the Cyclone Scheme compiler.

Commands:
  install - Fetch, build, and install a package from the remote repository
  query - Determine if the given package is installed
  sync - Download the latest package index from the remote repository
")
  (newline))

;; local-db:get :: string -> Either alist boolean
;; Get contents of the package file for an installed package, if found
;; Returns false if the package is not installed
(define (local-db:get name)
  (let ((pkg-file (string-append *local-repo-dir* "/" name "/package.scm")))
;(write `(DEBUG ,pkg-file))
    (cond
      ((not (file-exists? pkg-file))
       #f)
      (else 
        (call-with-input-file
          pkg-file
          (lambda (fp)
            (read-all fp)))))))

;; remote-db:get :: string -> Either alist boolean
;; Get information for a remote package, using the cached index.dat.
;; Returns false if the package is not found
(define (remote-db:get name)
  (let ((index-file (string-append *local-repo-dir* "/index.dat")))
    (cond
      ((not (file-exists? index-file))
       #f)
      (else
        (call-with-input-file
          index-file
          (lambda (fp)
            (and-let* ((index (read-all fp))
                       (entry (assoc (string->symbol name) index)))
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
       "mkdir -p " *local-repo-dir*)
     (download 
       (string-append *remote-repo-url* "index.dat")
       (string-append *local-repo-dir* "/index.dat")))
;; TODO: need a query command to query for installed packages
    ((equal? cmd "query")
     (let* ((package-name (cadr args))
            (package-info (local-db:get package-name)))
       (cond
         ((not package-info)
          ;; TODO: how to handle this case?
          (display "Package not found!"))
         (else
          (display package-name)
          (display "-")
          (display (cadr (assoc 'ver package-info)))
          (newline)))))
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
            (package-info (remote-db:get package-name)))
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
           *local-repo-dir*
           "/" package-name "/package.scm")))
    )
    ((equal? cmd "upgrade")
     ;; TODO: check local installed version vs version from index.dat
     ;;       - if later version in index.dat, proceed with upgrade
     ;;       - otherwise, quit with "no update available"
     (let* ((package-name (cadr args))
            (installed-package-info (local-db:get package-name))
            (remote-package-info (remote-db:get package-name)))
       (write `(local ,(assoc 'ver installed-package-info)
                remote ,(assoc 'ver remote-package-info))))
;; TODO: uninstall package (removes old version)
;; TODO: install package
    )
    ((member cmd '("install-local" "uninstall-local" "test"))
      (let ((cmd (car args))
            (pkgfile (cadr args)))
        (process-pkg cmd pkgfile)))
    (else
     (usage))))
