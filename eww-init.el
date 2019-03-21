(setq eww-search-prefix "https://duckduckgo.com/lite/?q=")
(setq my-search-engines-list
      '(("Słownik Języka Polskiego PWN" .
	 "https://sjp.pwn.pl/szukaj/")
	("Oxford Dictionary" .
	 "https://en.oxforddictionaries.com/definition/")
	("Glosbe Translate Pl-En" .
	 "https://glosbe.com/pl/en/")
	("Glosbe Translate En-Pl" .
	 "https://glosbe.com/en/pl/")
	("Wikipedia PL" .
	 "https://pl.wikipedia.org/wiki/Special:Search?search=")
	("Wikipedia EN" .
	 "https://en.wikipedia.org/wiki/Special:Search?search=")
	("The Pirate Bay" .
	 "https://thepiratebay.org/search/")))

(defun my-search-web ()
  (interactive)
  (helm
   :prompt "Search: "
   :sources `((
	       (name . "Search Engines")
	       (candidates . ,my-search-engines-list)
	       (action . (lambda (r)
			   (eww (concat r (read-string r)))))))))
(define-key eww-mode-map (kbd "s") 'my-search-web)
