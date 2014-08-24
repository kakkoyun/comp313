;; Author : Kemal Akkoyun
;; Student ID : 11076004
;; Source : Structure and Interpretation Of Computer Programs - MIT Press
;;          Christopher J. K. Stephenson's Comp 313 Lectures.
;; Information : Comp313 - Project 4

;; Guiseppe functions from previous weeks.

;; data definition
;; g-number is 
;; sıfır
;; OR
;; (s g-number)


;; Some g-number definitions.
(tanımla BİR (s sıfır))
(tanımla İKİ (s BİR))
(tanımla ÜÇ (s İKİ))
(tanımla DÖRT (s ÜÇ))
(tanımla BEŞ (s DÖRT))
(tanımla ALTI (s BEŞ))
(tanımla YEDİ (s ALTI))
(tanımla SEKİZ (s YEDİ))
(tanımla DOKUZ (s SEKİZ))
(tanımla ON (s DOKUZ))

;; Identity function for Guiseppe.
(tanımla ID (λ x x))

;; tekrar: (g-number->gnumber) g-number -> (g-number -> g-number)
;; To repeat given function, given g-number times and 
;;    produce a funtion that waiting to consume a g-number
(tanımla tekrar 
         (λ f
           (λ g1
             (eğer (sıfır? g1) 
                   ID
                   (λ g2 (f (((tekrar f) (ö g1)) g2)))))))

;; topla : gNumber gNumber -> gnumber
;; Purpose: to sum two g-number
(tanımla topla (tekrar s))

;; çıkart : gNumber gNumber -> gnumber
;; Purpose: to substract a g-number from another
(tanımla çıkart (tekrar ö))

;; kaldır : (g-number->g-number) g-number g-number g-number -> g-number
;; Purpose: a wrapper high-order function consume functions, generalize procedures.
(tanımla kaldır
         (λ f
           (λ nv
             (λ g1
               (λ g2
                 (((tekrar (f g1)) g2) nv))))))

;; çarp : gNumber gNumber -> gnumber
;; Purpose: to product two g-number
(tanımla çarp ((kaldır topla) sıfır))

;; üstü : g-number g-number -> g-number
;; Purpose: to expoantiation two g-number 
(tanımla üstü ((kaldır çarp) BİR))

;; knuth : g-number g-number -> g-number
;; Purpose: to produce large g-numbers
(tanımla knuth ((kaldır üstü) BİR))

;; eşit-mi? : g-number g-number -> doğru/yanlış
;; Purpose : To check equivalence of two g-numbers
(tanımla eşit-mi? (λ g1 (λ g2 (sıfır? ((çıkart g2) g1)))))

;; ... : doğru/yanlış doğru/yanlış -> doğru/yanlış
;; Purpose : Logical Operators fo g-Numbers
(tanımla değil (λ t (eğer t yanlış doğru)))
(tanımla ve (λ a (λ b (eğer a b yanlış))))
(tanımla veya (λ a (λ b (eğer a doğru b))))

;; Tests
((eşit-mi? ((topla BİR) sıfır)) BİR)
((eşit-mi? ((topla sıfır) BİR)) BİR)
((eşit-mi? ((topla ÜÇ) DÖRT)) YEDİ)

((eşit-mi? ((çıkart BİR) İKİ)) BİR)
((eşit-mi? ((çıkart sıfır) BİR)) BİR)
((eşit-mi? ((çıkart ÜÇ) DÖRT)) BİR)

((eşit-mi? ((çarp ÜÇ) sıfır)) sıfır)
((eşit-mi? ((çarp sıfır) ÜÇ)) sıfır)
((eşit-mi? ((çarp ÜÇ) BİR)) ÜÇ)
((eşit-mi? ((çarp BİR) ÜÇ)) ÜÇ)
((eşit-mi? ((çarp ÜÇ) İKİ)) ALTI)

((eşit-mi? ((üstü ÜÇ) sıfır)) BİR)
((eşit-mi? ((üstü sıfır) ÜÇ)) sıfır)
((eşit-mi? ((üstü ÜÇ) BİR)) ÜÇ)
((eşit-mi? ((üstü BİR) ÜÇ)) BİR)
((eşit-mi? ((üstü BİR) YEDİ)) BİR)
((eşit-mi? ((üstü BİR) ÜÇ)) BİR)
((eşit-mi? ((üstü İKİ) ÜÇ)) SEKİZ)
((eşit-mi? ((üstü ÜÇ) İKİ)) DOKUZ)

((eşit-mi? ((knuth BİR) ÜÇ)) BİR)
((eşit-mi? ((knuth ÜÇ) İKİ)) ((çarp DOKUZ) ÜÇ))
((eşit-mi? ((knuth İKİ) İKİ)) DÖRT)
((eşit-mi? ((knuth İKİ) ÜÇ)) ((çarp DÖRT) DÖRT))

;; ============================================================== ;;
;; ========================= Classwork ========================== ;;
;; ============================================================== ;;


;; böl/mod : (g-number->g-number) g-number g-number g-number -> (g-number->g-number)
;; Purpose : A higher-order function for modulo and division.
(tanımla böl/mod
         (λ devam
           (λ bölen
             (λ bölüm
               (λ kalan
                 (eğer (sayı-mı? ((çıkart bölen) kalan))
                       ((((böl/mod devam) bölen) (s bölüm)) ((çıkart bölen) kalan))
                       ((devam bölüm) kalan)))))))

;; böl: g-number g-number g-number -> g-number
;; Purpose : To divide two g-numbers.
(tanımla böl 
         (böl/mod (λ x (λ y x))))


; Tests:
((eşit-mi? (((böl BİR) sıfır) BİR)) BİR)
((eşit-mi? (((böl İKİ) sıfır) ALTI)) ÜÇ)
((eşit-mi? (((böl BİR) sıfır) DÖRT)) DÖRT)
((eşit-mi? (((böl ÜÇ) sıfır) ALTI)) İKİ)

;; mod: g-number g-number g-number -> g-number
;; Purpose : To take modulo of given g-number with a base of another g-number.
(tanımla mod 
         (böl/mod (λ x (λ y y))))

;; Tests:
((eşit-mi? (((mod ÜÇ) sıfır) ÜÇ)) sıfır)
((eşit-mi? (((mod İKİ) sıfır) ALTI)) sıfır)
((eşit-mi? (((mod DÖRT) sıfır) YEDİ)) ÜÇ)
((eşit-mi? (((mod ÜÇ) sıfır) ON)) BİR)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists And List Operations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; çift : x x -> (x x)
;; Purpose : A constructor for making pairs.
(tanımla çift 
         (λ a (λ b (λ z (eğer z a b)))))

;; baş: çift(x y) -> x
;; Purpose : To access first element of given pair.
(tanımla baş 
         (λ ç (ç doğru)))

;; baş: çift(x y) -> y
;; Purpose : To access second element of given pair.
(tanımla kuyruk 
         (λ ç (ç yanlış)))

;; ata : (x->y) çiftOfx -> çiftOfy
;; Prupose: a map implemetation over guiseppe çift's.
(tanımla ata-basit
         (λ f
           (λ ç
             (eğer (sıfır? ç)
                   sıfır
                   ((çift (f (baş ç))) ((ata-basit f) (kuyruk ç)))))))

;; katla : (x->y) y çift -> y
;; Prupose: a fold implemetation over guiseppe çift's.
(tanımla katla
         (λ birleş
           (λ sıfır-değer
             (λ ç
               (eğer (sıfır? ç)
                     sıfır-değer
                     ((birleş (baş ç)) (((katla birleş) sıfır-değer) (kuyruk ç))))))))

;; Data Definition
;; positional-number is
;; ((çift g-number) g-number)
;; or
;; ((çift g-number) çift)

;; p->g : g-number positional-number -> g-number
;; Purpose: To convert positional-number to a g-number with given base.
(tanımla p->g
         (λ base
           (λ p
             (((katla 
                (λ l (λ r ((topla l) ((çarp base) r)))))
               sıfır)
              p))))

;; Tests:
((eşit-mi? ((p->g İKİ) ((çift BİR) sıfır))) BİR)
((eşit-mi? ((p->g İKİ) ((çift BİR)((çift BİR) sıfır)))) ÜÇ)
((eşit-mi? ((p->g İKİ) ((çift BİR)((çift BİR)((çift BİR) sıfır))))) YEDİ)

;; g->p : g-number g-number -> positional-number
;; Purpose: To convert g-number to a positional-number with given base.
(tanımla g->p
         (λ base
           (λ g
             (eğer (sıfır? g) sıfır
                   ((((böl/mod
                       (λ bölüm (λ kalan ((çift kalan) ((g->p base) bölüm)))))
                      base)
                     sıfır) g)))))

;; Tests:
((eşit-mi? ((p->g İKİ)((g->p İKİ) ALTI)))((p->g İKİ) ((çift sıfır)((çift BİR)((çift BİR) sıfır)))))
((eşit-mi? ((p->g İKİ)((g->p İKİ) DÖRT)))((p->g İKİ) ((çift sıfır)((çift sıfır)((çift BİR) sıfır)))))
((eşit-mi? ((p->g İKİ)((g->p İKİ) YEDİ)))((p->g İKİ) ((çift BİR)((çift BİR)((çift BİR) sıfır)))))

;; helper/wrapper functions for processing cift's
;; check null/empty/sıfır or first 
(tanımla baş-nv 
         (λ nv (λ ç (eğer (sıfır? ç) nv (baş ç)))))
;; check null/empty/sıfır or rest
(tanımla kuyruk-nv 
         (λ ç (eğer (sıfır? ç) sıfır (kuyruk ç))))


;; ata-gelişmiş : (x->y) çiftOfx -> çiftOfy
;; Prupose: a map implemetation over guiseppe çift's. IMPROVED VERSION:
(tanımla ata-gelişmiş
         (λ nv
           (λ f
             (λ ç1
               (λ ç2
                 (eğer ((ve (sıfır? ç1)) (sıfır? ç2)) sıfır
                       ((çift ((f ((baş-nv nv)ç1)) ((baş-nv nv) ç2)))
                        ((((ata-gelişmiş nv) f) (kuyruk-nv ç1))(kuyruk-nv ç2))))))))) 

;; normalize: g-number g-number positional-number -> positional-number
;; Purpose: Take an unnormalized positional number and normalize it by eleminating all digits which are larger than base
;; and doig carries.
(tanımla normalize
         (λ elde
           (λ base
             (λ ç
               (eğer ((ve (sıfır? ç)) (sıfır? elde)) sıfır
                     ((((böl/mod (λ bölüm (λ kalan ((çift kalan) (((normalize bölüm)base) (kuyruk-nv ç))))))
                        base)sıfır)
                      ((topla elde) ((baş-nv sıfır) ç)))
                     )))))

;; topla-p : g-number psoitional-number positional-number -> positional-number
;; Purpose: To sum two positional numbers.
(tanımla topla-p 
         (λ base 
           (λ p1 
             (λ p2 
               (((normalize sıfır) base) ((((ata-gelişmiş sıfır) topla) p1) p2))))))

;; Test:
((eşit-mi? ((p->g İKİ)(((topla-p İKİ)((g->p İKİ) ALTI)) ((g->p İKİ) DÖRT)))) ON)
((eşit-mi? ((p->g İKİ)(((topla-p İKİ)((g->p İKİ) BİR)) ((g->p İKİ) ÜÇ)))) DÖRT)
((eşit-mi? ((p->g İKİ)(((topla-p İKİ)((g->p İKİ) ALTI)) ((g->p İKİ) İKİ)))) SEKİZ)

;; çarp-p-g : g-number psoitional-number g-number -> positional-number
;; Purpose: To product a positional number with a g-number.
(tanımla çarp-p-g 
         (λ base 
           (λ p 
             (λ g 
               (((normalize sıfır) base)((ata-basit (çarp g)) p))))))

;; Test:
((eşit-mi? ((p->g İKİ)(((çarp-p-g İKİ)((g->p İKİ) ALTI)) DÖRT )))((çarp İKİ)((topla ON) İKİ)))
((eşit-mi? ((p->g İKİ)(((çarp-p-g İKİ)((g->p İKİ) BİR)) İKİ ))) İKİ)
((eşit-mi? ((p->g İKİ)(((çarp-p-g İKİ)((g->p İKİ) ALTI)) BİR ))) ALTI)

;; çarp-p-p : g-number psoitional-number g-number -> positional-number
;; Purpose: To product two positional numbers.
(tanımla çarp-p-p 
         (λ base 
           (λ p1 
             (λ p2 
               (((katla 
                  (λ l (λ r
                         (((topla-p base)
                           (((çarp-p-g base) p1) l))
                          ((çift sıfır) r)))))
                 sıfır) p2)))))

;; Test:
((eşit-mi? ((p->g İKİ)(((çarp-p-p İKİ)((g->p İKİ) ALTI)) ((g->p İKİ) DÖRT))))((çarp İKİ)((topla ON) İKİ)))
((eşit-mi? ((p->g İKİ)(((çarp-p-p İKİ)((g->p İKİ) BİR)) ((g->p İKİ) ÜÇ)))) ÜÇ)
((eşit-mi? ((p->g İKİ)(((çarp-p-p İKİ)((g->p İKİ) ALTI)) ((g->p İKİ) İKİ))))((topla ON) İKİ))


;; çift-göster: çift -> g-number
;; Purpose: To show positional number as g-numbers
(tanımla çift-göster
         (λ ç
           ((ata-basit göster) ç)))

;; Tests:
(çift-göster(((topla-p İKİ)((g->p İKİ) BİR)) ((g->p İKİ) ÜÇ)))
(çift-göster (((topla-p ON) ((çift BEŞ) ((çift ÜÇ) ((çift BİR) sıfır)))) ((çift ALTI) ((çift DOKUZ) sıfır))))
(çift-göster(((çarp-p-g İKİ)((g->p İKİ) BİR)) İKİ ))
(çift-göster (((çarp-p-p SEKİZ) ((çift DÖRT) ((çift ÜÇ) ((çift YEDİ) sıfır))))
              ((çift ALTI) ((çift ON) ((çift İKİ) sıfır)))))
(çift-göster(((çarp-p-p İKİ)((g->p İKİ) ALTI)) ((g->p İKİ) DÖRT)))
(çift-göster(((çarp-p-p İKİ)((g->p İKİ) BİR)) ((g->p İKİ) ÜÇ)))