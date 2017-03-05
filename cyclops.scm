(import 
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme file)
  (scheme cyclone libraries)
  (scheme cyclone transforms)
  (scheme cyclone util)
)

;; TODO: constants to later externalize
(define *cyclone-repo-dir* ".") ;; TODO: temporarily use local for testing
(define *pkg-file-dir* "../cyclone-packages/sample-lib")
(define *pkg-file* "../cyclone-packages/sample-lib/package.scm")

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
             (lib? (file-is-library? file/path))
             (file/path-no-ext (basename file/path)))
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
               (list 
                file/path
                (string-append file/path-no-ext ".o")
                (string-append file/path-no-ext ".meta")
               )))) ;; TODO: possibly any included .scm files, too
        (else
          (set! install-file-list 
             (append 
               install-file-list 
               (list 
                file/path-no-ext ;; compiled executable
               )))))
    ))
    (get-file-list params))

  ;; TODO: Install files
(write `(files to install: ,install-file-list)) (newline) ;; more DEBUGGING
  (for-each
    (lambda (filename)
      ; Strip off leading directory
      (let* ((pkg-basedir-len (string-length *pkg-file-dir*))
             (fn-no-base (substring filename pkg-basedir-len (string-length filename))))
        (write `(DEBUG ,fn-no-base))
      )

      ;(system 
      ;  (string-append "cp " filename " " *cyclone-repo-dir*))
    )
    install-file-list)

  ;; Run the 'install' section, if applicable 
  (let ((install-directive (assoc 'install params)))
    (if install-directive
        (for-each
          (lambda (cmd)
            (system cmd))
          (cdr install-directive))))
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
            (system 
              (string-append "mkdir " dir))))
       path)))

#;(call-with-input-file 
  *pkg-file*
  (lambda (fp)
    (read-pkg fp)
  ))

#;(write
  (split "/scheme/cyclone/sample.sld" #\/))

(write
  (create-missing-dirs "scheme/cyclone/sample.sld" "."))
