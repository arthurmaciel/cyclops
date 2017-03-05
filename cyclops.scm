(import 
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme file)
  (scheme cyclone libraries)
)

;; TODO: constants to later externalize
(define *cyclone-repo-dir* ".") ;; TODO: temporarily use local for testing
(define *pkg-file-dir* "../cyclone-packages/sample-lib")
(define *pkg-file* "../cyclone-packages/sample-lib/package.scm")

;; Read given file and determine if it is a library
(define (file-is-library? filename)
  (call-with-input-file filename (lambda (fp)
    (let ((contents (read fp)))
      (library? contents)))))

;; Read all of the parameters from the given package file port
(define (read-pkg fp)
  (let ((params (read-all fp)))
    (install params)))

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

;; Build & Install files using the given package file parameters
(define (install params)
  (define install-file-list '())
  ;; Build files
  (for-each
    (lambda (file)
      (let* ((file/path (string-append *pkg-file-dir* "/" file))
             (lib? (file-is-library? file/path)))
(write `(,file/path ,(file-is-library? file/path)))(newline) ;; DEBUG
        (system 
          (string-append 
            "cyclone " 
            file/path))
      ;; TODO: check return code, make sure build succeeded
      ;; TODO: if library, install .o .sld .meta files
      (cond
        (lib?
          (set! install-file-list 
             (append 
               install-file-list 
               (list file/path)))) ;; TODO: .o .meta files. possibly any included .scm files, too
        (else
          (set! install-file-list 
             (append 
               install-file-list 
               (list file/path))))) ;; TODO: compiled executable
    ))
    (get-file-list params))

  ;; TODO: Install files
(write `(DEBUG ,install-file-list)) (newline) ;; more DEBUGGING

  ;; Run the 'install' section, if applicable 
  (let ((install-directive (assoc 'install params)))
    (if install-directive
        (for-each
          (lambda (cmd)
            (system cmd))
          (cdr install-directive))))
  )

(call-with-input-file 
  *pkg-file*
  (lambda (fp)
    (read-pkg fp)
  ))
