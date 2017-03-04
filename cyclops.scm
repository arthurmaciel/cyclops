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

(define (read-pkg fp)
  (let ((params (read-all fp)))
    (install params)))

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

(define (install params)
  (for-each
    (lambda (file)
      (write file)(newline)
      (system 
        (string-append 
          "cyclone " 
          *pkg-file-dir*
          "/"
          file))
    )
    (get-file-list params)))

(call-with-input-file 
  *pkg-file*
  (lambda (fp)
    (read-pkg fp)
  ))
