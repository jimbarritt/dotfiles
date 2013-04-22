;; put something different in the scratch buffer

;; See here: http://patorjk.com/software/taag/#p=display&h=1&v=1&f=Doh&t=Emacs

(defconst *emacs* ";;                                                                                                    
;; EEEEEEEEEEEEEEEEEEEEEE                                                                              
;; E::::::::::::::::::::E                                                                              
;; E::::::::::::::::::::E                                                                              
;; EE::::::EEEEEEEEE::::E                                                                              
;;   E:::::E       EEEEEE   mmmmmmm    mmmmmmm     aaaaaaaaaaaaa      cccccccccccccccc    ssssssssss   
;;   E:::::E              mm:::::::m  m:::::::mm   a::::::::::::a   cc:::::::::::::::c  ss::::::::::s  
;;   E::::::EEEEEEEEEE   m::::::::::mm::::::::::m  aaaaaaaaa:::::a c:::::::::::::::::css:::::::::::::s 
;;   E:::::::::::::::E   m::::::::::::::::::::::m           a::::ac:::::::cccccc:::::cs::::::ssss:::::s
;;   E:::::::::::::::E   m:::::mmm::::::mmm:::::m    aaaaaaa:::::ac::::::c     ccccccc s:::::s  ssssss 
;;   E::::::EEEEEEEEEE   m::::m   m::::m   m::::m  aa::::::::::::ac:::::c                s::::::s      
;;   E:::::E             m::::m   m::::m   m::::m a::::aaaa::::::ac:::::c                   s::::::s   
;;   E:::::E       EEEEEEm::::m   m::::m   m::::ma::::a    a:::::ac::::::c     cccccccssssss   s:::::s 
;; EE::::::EEEEEEEE:::::Em::::m   m::::m   m::::ma::::a    a:::::ac:::::::cccccc:::::cs:::::ssss::::::s
;; E::::::::::::::::::::Em::::m   m::::m   m::::ma:::::aaaa::::::a c:::::::::::::::::cs::::::::::::::s 
;; E::::::::::::::::::::Em::::m   m::::m   m::::m a::::::::::aa:::a cc:::::::::::::::c s:::::::::::ss  
;; EEEEEEEEEEEEEEEEEEEEEEmmmmmm   mmmmmm   mmmmmm  aaaaaaaaaa  aaaa   cccccccccccccccc  sssssssssss 
;; 
;; ")

(defconst *jmdb* ";;                                                                                  
;;                                                      ddddddddbbbbbbbb            
;;             jjjj                                     d::::::db::::::b            
;;            j::::j                                    d::::::db::::::b            
;;             jjjj                                     d::::::db::::::b            
;;                                                      d:::::d  b:::::b            
;;           jjjjjjj   mmmmmmm    mmmmmmm       ddddddddd:::::d  b:::::bbbbbbbbb    
;;           j:::::j mm:::::::m  m:::::::mm   dd::::::::::::::d  b::::::::::::::bb  
;;            j::::jm::::::::::mm::::::::::m d::::::::::::::::d  b::::::::::::::::b 
;;            j::::jm::::::::::::::::::::::md:::::::ddddd:::::d  b:::::bbbbb:::::::b
;;            j::::jm:::::mmm::::::mmm:::::md::::::d    d:::::d  b:::::b    b::::::b
;;            j::::jm::::m   m::::m   m::::md:::::d     d:::::d  b:::::b     b:::::b
;;            j::::jm::::m   m::::m   m::::md:::::d     d:::::d  b:::::b     b:::::b
;;            j::::jm::::m   m::::m   m::::md:::::d     d:::::d  b:::::b     b:::::b
;;            j::::jm::::m   m::::m   m::::md::::::ddddd::::::dd b:::::bbbbbb::::::b
;;            j::::jm::::m   m::::m   m::::m d:::::::::::::::::d b::::::::::::::::b 
;;            j::::jm::::m   m::::m   m::::m  d:::::::::ddd::::d b:::::::::::::::b  
;;            j::::jmmmmmm   mmmmmm   mmmmmm   ddddddddd   ddddd bbbbbbbbbbbbbbbb   
;;            j::::j                                                                
;;  jjjj      j::::j                                                                
;; j::::jj   j:::::j                                                                
;; j::::::jjj::::::j                                                                
;;  jj::::::::::::j                                                                 
;;    jjj::::::jjj                                                                  
;;       jjjjjj                                                                     
;;
;; ")


;; TODO - Make a function that calls this webservice to generate these automatically and insert to the buffer

(defun choose-message () 
  "Time to start coding ...")
(defun current-user ()
  (first (split-string (shell-command-to-string "whoami") "\n")))

     
(setq initial-scratch-message      
      (concat *emacs*
(first (split-string (version) "\n"))
"
;;
;; Hello " (current-user) ". " (choose-message)
"
"))



