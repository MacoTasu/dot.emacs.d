; alc:検索文字列 でalcで検索できるように
(eval-after-load "w3m-search"
  '(progn
     (add-to-list 'w3m-search-engine-alist
                '("alc"
                  "http://eow.alc.co.jp/%s/UTF-8/"
                  utf-8))
     (add-to-list 'w3m-uri-replace-alist
                  '("\`alc:" w3m-search-uri-replace "alc"))))

(require 'w3m-search)
