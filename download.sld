(define-library (download)
  (import 
    (scheme base)
    (scheme write))
  (include-c-header "download-header.h")
  (include-c-header "<curl/curl.h>")
  (export
    download)
  (begin

(define tmp 1) ;; TODO: cyclone bug that this is required

(define-c download
  "(void *data, int argc, closure _, object k, object url, object filename)"
  " 
  CURL *curl_handle;
  FILE *pagefile;

  Cyc_check_str(data, url);
  Cyc_check_str(data, filename);
  curl_global_init(CURL_GLOBAL_ALL);

  /* init the curl session */
  curl_handle = curl_easy_init();

  /* set URL to get here */
  curl_easy_setopt(curl_handle, CURLOPT_URL, string_str(url));

  /* Switch on full protocol/debug output while testing */
  //curl_easy_setopt(curl_handle, CURLOPT_VERBOSE, 1L);

  /* disable progress meter, set to 0L to enable and disable debug output */
  curl_easy_setopt(curl_handle, CURLOPT_NOPROGRESS, 0L);

  /* send all data to this function  */
  curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_data);

  /* open the file */
  pagefile = fopen(string_str(filename), \"wb\");
  if(pagefile) {

    /* write the page body to this file handle */
    curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, pagefile);

    /* get it! */
    curl_easy_perform(curl_handle);

    /* close the header file */
    fclose(pagefile);
  }

  /* cleanup curl stuff */
  curl_easy_cleanup(curl_handle);

  return_closcall1(data, k, boolean_t);
    ")

))
