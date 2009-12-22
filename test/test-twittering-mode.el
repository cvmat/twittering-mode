
;; (deftest case-string twittering-mode
;;   (assert
;;    (case-string "Rinko"
;;      (("Rinko") t)
;;      (t nil)))
;;   (assert
;;    (case-string "Rinko"
;;      (("Manaka") nil)
;;      (("Nene" "Rinko") t)
;;      (t nil)))
;;   (assert
;;    (case-string "Nene"
;;      (("Manaka" "Rinko") nil)
;;      (t t)))
;;   )

(defcase percent-encode nil nil
  (test-assert-string-equal "Rinko"
    (twittering-percent-encode "Rinko"))
  
  (test-assert-string-equal "%25"
    (twittering-percent-encode "%"))
  
  (test-assert-string-equal "love+plus"
    (twittering-percent-encode "love plus")))

(defcase case-string nil nil
  (test-assert-string-equal "Kobayakawa"
    (case-string "Rinko"
      (("Rinko") "Kobayakawa")
      (t "unknown")))

  (test-assert-string-equal "unknown"
    (case-string "Manaka"
      (("Rinko") "Kobayakawa")
      (t "unknown")))
  
  (test-assert-string-equal "Kobayakawa"
    (case-string "Rinko"
      (("Manaka") "Takane")
      (("Rinko") "Kobayakawa")
      (("Nene") "Anegasaki")
      (t nil)))

  (test-assert-string-equal "Amphibian"
    (case-string "Frog"
      (("Rabbit") "Mammal")
      (("Salamandar" "Frog") "Amphibian")
      (t nil)))
  )

(defcase format-string nil nil
  (test-assert-string-equal ""
    (twittering-format-string "" "" nil))

  (test-assert-string-equal "Hello world"
    (twittering-format-string "Hello world" "" nil))

  (test-assert-string-equal "RT: twittering-mode now (via @twmode)"
    (twittering-format-string "RT: %t (via @%s)" "%"
			      '(("t" . "twittering-mode now")
				("s" . "twmode"))))

  (test-assert-string-equal "RT: %t (via @twmode)"
    (twittering-format-string "RT: %t (via @%s)" "%"
			      '(("t" . "%t")
				("s" . "twmode"))))

  (test-assert-string-equal "new\nline"
    (twittering-format-string "new~%line" "~"
			      '(("%" . "\n"))))
  )

(defcase sign-string nil nil
    (setq twittering-sign-simple-string nil)
    (test-assert-string-equal ""
      (twittering-sign-string))

    (setq twittering-sign-simple-string "")
    (test-assert-string-equal " []"
      (twittering-sign-string))

    (setq twittering-sign-simple-string "foo")
    (test-assert-string-equal " [foo]"
      (twittering-sign-string))

    (setq twittering-sign-string-function (lambda () "foo"))
    (test-assert-string-equal "foo"
      (twittering-sign-string))
  )

(defcase timeline-spec nil nil
  (defun test-restore-timeline-spec(spec)
    (string-equal
     spec
     (apply 'twittering-get-timeline-spec
            (twittering-get-host-method-from-timeline-spec spec))))
  (defun test-restore-host-method(host method)
    (equal (list host method)
            (twittering-get-host-method-from-timeline-spec
             (twittering-get-timeline-spec host method))))
  (test-assert-ok (test-restore-timeline-spec "~"))
  (test-assert-ok (test-restore-timeline-spec "@"))
  (test-assert-ok (test-restore-timeline-spec "M"))
  (test-assert-ok (test-restore-timeline-spec "-"))
  (test-assert-ok (test-restore-timeline-spec "USER"))
  (test-assert-ok (test-restore-timeline-spec "USER/LISTNAME"))
  (test-assert-ok (test-restore-timeline-spec "H"))
  (test-assert-ok (test-restore-timeline-spec "B"))
  (test-assert-ok (test-restore-timeline-spec "T"))
  (test-assert-ok (test-restore-timeline-spec "O"))

  (test-assert-eq nil (twittering-get-host-method-from-timeline-spec ""))

  (test-assert-ok
   (test-restore-host-method
    "twitter.com" "statuses/friends_timeline"))
  (test-assert-ok
   (test-restore-host-method
    "twitter.com" "statuses/replies"))
  (test-assert-ok
   (test-restore-host-method
    "twitter.com" "statuses/mentions"))
  (test-assert-ok
   (test-restore-host-method
    "twitter.com" "statuses/public_timeline"))
  (test-assert-ok
   (test-restore-host-method
    "twitter.com" "statuses/user_timeline/USER"))
  (test-assert-ok
   (test-restore-host-method
    "api.twitter.com" "1/USER/lists/LISTNAME/statuses"))
  (test-assert-ok
   (test-restore-host-method
    "api.twitter.com" "1/statuses/home_timeline"))
  (test-assert-ok
   (test-restore-host-method
    "api.twitter.com" "1/statuses/retweeted_by_me"))
  (test-assert-ok
   (test-restore-host-method
    "api.twitter.com" "1/statuses/retweeted_to_me"))
  (test-assert-ok
   (test-restore-host-method
    "api.twitter.com" "1/statuses/retweets_of_me"))

  (test-assert-string-equal
   "USERNAME"
   (let ((twittering-username-active "USERNAME"))
     (twittering-get-timeline-spec
      "twitter.com" "statuses/user_timeline")))
  )
