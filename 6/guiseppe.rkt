;; Author : Kemal Akkoyun
;; Student ID : 11076004
;; Source : Christopher J. K. Stephenson's Comp 313 Lectures and Notes.
;; Information : Comp313 - Project 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; MOST RELEVANT CODES ARE AT THE END. ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Guiseppe functions from previous weeks.

;; data definition
;; g-sayı is 
;; sıfır
;; OR
;; (s g-sayı)


;; Some g-sayı definitions.
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

;; tekrar: (g-sayı->gsayı) g-sayı -> (g-sayı -> g-sayı)
;; To repeat given function, given g-sayı times and 
;;    produce a funtion that waiting to consume a g-sayı
(tanımla tekrar 
         (λ f
           (λ g1
             (eğer (sıfır? g1) 
                   ID
                   (λ g2 (f (((tekrar f) (ö g1)) g2)))))))

;; topla : g-sayı g-sayı -> g-sayı
;; Purpose: to sum two g-sayı
(tanımla topla (tekrar s))

;; çıkart : g-sayı g-sayı -> g-sayı
;; Purpose: to substract a g-sayı from another
(tanımla çıkart (tekrar ö))

;; kaldır : (g-sayı->g-sayı) g-sayı g-sayı g-sayı -> g-sayı
;; Purpose: a wrapper high-order function consume functions, generalize procedures.
(tanımla kaldır
         (λ f
           (λ sıfır-değer
             (λ g1
               (λ g2
                 (((tekrar (f g1)) g2) sıfır-değer))))))

;; çarp : g-sayı g-sayı -> g-sayı
;; Purpose: to product two g-sayı
(tanımla çarp ((kaldır topla) sıfır))

;; üstü : g-sayı g-sayı -> g-sayı
;; Purpose: to expoantiation two g-sayı 
(tanımla üstü ((kaldır çarp) BİR))

;; knuth : g-sayı g-sayı -> g-sayı
;; Purpose: to produce large g-sayı
(tanımla knuth ((kaldır üstü) BİR))

;; eşit-mi? : g-sayı g-sayı -> doğru/yanlış
;; Purpose : To check equivalence of two g-sayı
(tanımla eşit-mi? (λ g1 (λ g2 (sıfır? ((çıkart g2) g1)))))

;; ... : doğru/yanlış doğru/yanlış -> doğru/yanlış
;; Purpose : Logical Operators fo g-sayı
(tanımla değil (λ t (eğer t yanlış doğru)))
(tanımla ve (λ a (λ b (eğer a b yanlış))))
(tanımla veya (λ a (λ b (eğer a doğru b))))

;; Tests:
"Basic g-sayı arithmetics"
(kaç-ö)
(kaç-s)

;; Tests
"topla"
((eşit-mi? ((topla BİR) sıfır)) BİR)
((eşit-mi? ((topla sıfır) BİR)) BİR)
((eşit-mi? ((topla ÜÇ) DÖRT)) YEDİ)
(kaç-ö)
(kaç-s)

"çıkart"
((eşit-mi? ((çıkart BİR) İKİ)) BİR)
((eşit-mi? ((çıkart sıfır) BİR)) BİR)
((eşit-mi? ((çıkart ÜÇ) DÖRT)) BİR)
(kaç-ö)
(kaç-s)

"çarp"
((eşit-mi? ((çarp ÜÇ) sıfır)) sıfır)
((eşit-mi? ((çarp sıfır) ÜÇ)) sıfır)
((eşit-mi? ((çarp ÜÇ) BİR)) ÜÇ)
((eşit-mi? ((çarp BİR) ÜÇ)) ÜÇ)
((eşit-mi? ((çarp ÜÇ) İKİ)) ALTI)
(kaç-ö)
(kaç-s)

"üssü"
((eşit-mi? ((üstü ÜÇ) sıfır)) BİR)
((eşit-mi? ((üstü sıfır) ÜÇ)) sıfır)
((eşit-mi? ((üstü ÜÇ) BİR)) ÜÇ)
((eşit-mi? ((üstü BİR) ÜÇ)) BİR)
((eşit-mi? ((üstü BİR) YEDİ)) BİR)
((eşit-mi? ((üstü BİR) ÜÇ)) BİR)
((eşit-mi? ((üstü İKİ) ÜÇ)) SEKİZ)
((eşit-mi? ((üstü ÜÇ) İKİ)) DOKUZ)
(kaç-ö)
(kaç-s)

"knuth"
((eşit-mi? ((knuth BİR) ÜÇ)) BİR)
(kaç-ö)
(kaç-s)
((eşit-mi? ((knuth ÜÇ) İKİ)) ((çarp DOKUZ) ÜÇ))
(kaç-ö)
(kaç-s)
((eşit-mi? ((knuth İKİ) İKİ)) DÖRT)
(kaç-ö)
(kaç-s)
((eşit-mi? ((knuth İKİ) ÜÇ)) ((çarp DÖRT) DÖRT))
(kaç-ö)
(kaç-s)

;; böl/mod : (g-sayı->g-sayı) g-sayı g-sayı g-sayı -> (g-sayı->g-sayı)
;; Purpose : A higher-order function for modulo and division.
(tanımla böl/mod
         (λ devam
           (λ bölen
             (λ bölüm
               (λ kalan
                 (eğer (sayı-mı? ((çıkart bölen) kalan))
                       ((((böl/mod devam) bölen) (s bölüm)) ((çıkart bölen) kalan))
                       ((devam bölüm) kalan)))))))

;; böl: g-sayı g-sayı g-sayı -> g-sayı
;; Purpose : To divide two g-sayı
(tanımla böl 
         (böl/mod (λ x (λ y x))))

;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

; Tests:
"Böl"
((eşit-mi? (((böl BİR) sıfır) BİR)) BİR)
(kaç-ö)
(kaç-s)
((eşit-mi? (((böl İKİ) sıfır) ALTI)) ÜÇ)
(kaç-ö)
(kaç-s)
((eşit-mi? (((böl BİR) sıfır) DÖRT)) DÖRT)
(kaç-ö)
(kaç-s)
((eşit-mi? (((böl ÜÇ) sıfır) ALTI)) İKİ)
(kaç-ö)
(kaç-s)

;; mod: g-sayı g-sayı g-sayı -> g-sayı
;; Purpose : To take modulo of given g-sayı with a base of another g-sayı.
(tanımla mod 
         (böl/mod (λ x (λ y y))))

;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

;; Tests:
"Mod"
((eşit-mi? (((mod ÜÇ) sıfır) ÜÇ)) sıfır)
(kaç-ö)
(kaç-s)
((eşit-mi? (((mod İKİ) sıfır) ALTI)) sıfır)
(kaç-ö)
(kaç-s)
((eşit-mi? (((mod DÖRT) sıfır) YEDİ)) ÜÇ)
(kaç-ö)
(kaç-s)
((eşit-mi? (((mod ÜÇ) sıfır) ON)) BİR)
(kaç-ö)
(kaç-s)

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
;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

; Tests:
"Katla"
((eşit-mi? (((katla topla) sıfır) ((çift İKİ) ((çift BİR) ((çift ÜÇ) sıfır))))) ALTI)
;; Performance:
(kaç-ö)
(kaç-s)
((eşit-mi? (((katla çarp) BİR) ((çift İKİ) ((çift BİR) ((çift ÜÇ) sıfır))))) ALTI)
;; Performance:
(kaç-ö)
(kaç-s)

;; Data Definition
;; positional number
;; p-sayı is
;; ((çift g-sayı) g-sayı)
;; or
;; ((çift g-sayı) çift)

;; p->g : g-sayı p-sayı -> g-sayı
;; Purpose: To convert p-sayı to a g-sayı with given base.
(tanımla p->g
         (λ taban
           (λ p
             (((katla 
                (λ sol (λ sağ ((topla sol) ((çarp taban) sağ)))))
               sıfır)
              p))))

;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

;; Tests:
"p->g"
((eşit-mi? ((p->g İKİ) ((çift BİR) sıfır))) BİR)
(kaç-ö)
(kaç-s)
((eşit-mi? ((p->g İKİ) ((çift BİR)((çift BİR) sıfır)))) ÜÇ)
(kaç-ö)
(kaç-s)
((eşit-mi? ((p->g İKİ) ((çift BİR)((çift BİR)((çift BİR) sıfır))))) YEDİ)
(kaç-ö)
(kaç-s)

;; g->p : g-sayı g-sayı -> p-sayı
;; Purpose: To convert g-sayı to a p-sayı with given base.
(tanımla g->p
         (λ taban
           (λ g
             (eğer (sıfır? g) sıfır
                   ((((böl/mod
                       (λ bölüm (λ kalan ((çift kalan) ((g->p taban) bölüm)))))
                      taban)
                     sıfır) g)))))
;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

;; Tests:
"g->p"
((eşit-mi? ((p->g İKİ)((g->p İKİ) ALTI)))((p->g İKİ) ((çift sıfır)((çift BİR)((çift BİR) sıfır)))))
(kaç-ö)
(kaç-s)
((eşit-mi? ((p->g İKİ)((g->p İKİ) DÖRT)))((p->g İKİ) ((çift sıfır)((çift sıfır)((çift BİR) sıfır)))))
(kaç-ö)
(kaç-s)
((eşit-mi? ((p->g İKİ)((g->p İKİ) YEDİ)))((p->g İKİ) ((çift BİR)((çift BİR)((çift BİR) sıfır)))))
(kaç-ö)
(kaç-s)

;; helper/wrapper functions for processing ragged cift's
;; check null/empty/sıfır or first 
(tanımla baş-nv 
         (λ sıfır-değer (λ ç (eğer (sıfır? ç) sıfır-değer (baş ç)))))
;; check null/empty/sıfır or rest
(tanımla kuyruk-nv 
         (λ ç (eğer (sıfır? ç) sıfır (kuyruk ç))))


;; ata-gelişmiş : (x->y) çiftOfx -> çiftOfy
;; Prupose: a map implemetation over guiseppe çift's. IMPROVED VERSION:
(tanımla ata-gelişmiş
         (λ sıfır-değer
           (λ f
             (λ ç1
               (λ ç2
                 (eğer ((ve (sıfır? ç1)) (sıfır? ç2)) sıfır
                       ((çift ((f ((baş-nv sıfır-değer)ç1)) ((baş-nv sıfır-değer) ç2)))
                        ((((ata-gelişmiş sıfır-değer) f) (kuyruk-nv ç1))(kuyruk-nv ç2))))))))) 

; çift-eşit-mi? : çift öift -> doğru/yanlış
; Purpose : To check equivalence of two çift

(tanımla çift-eşit-mi?
           (λ ç1 
             (λ ç2 
               (((katla ve) doğru) ((ata-basit sıfır?) ((((ata-gelişmiş sıfır) çıkart) ç1) ç2))))))

;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

;; Tests:
"çift-eşit-mi?"
((çift-eşit-mi? ((çift BİR) sıfır)) ((çift BİR) sıfır))
"Expectation: Yanlış"((çift-eşit-mi? ((çift BİR) sıfır)) ((çift İKİ) sıfır))
;; Performance:
(kaç-ö)
(kaç-s)

;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

;; TESTS FOR LIST OPERATIONS:
"Ata-Basit"
((çift-eşit-mi? ((ata-basit (topla BİR)) ((çift İKİ) ((çift İKİ) sıfır)))) ((çift ÜÇ) ((çift ÜÇ) sıfır)))
;; Performance:
(kaç-ö)
(kaç-s)
((çift-eşit-mi? ((ata-basit (çarp İKİ)) ((çift İKİ) ((çift İKİ) sıfır)))) ((çift DÖRT) ((çift DÖRT) sıfır)))
;; Performance:
(kaç-ö)
(kaç-s)

;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

"ata-gelişmiş"
((çift-eşit-mi? ((((ata-gelişmiş sıfır) çıkart) ((çift BİR) ((çift İKİ) sıfır))) 
                  ((çift İKİ) ((çift SEKİZ) sıfır)))) ((çift BİR) ((çift ALTI) sıfır)))
(kaç-ö)
(kaç-s)
((çift-eşit-mi? ((((ata-gelişmiş sıfır) topla) ((çift BİR) ((çift İKİ) sıfır))) ((çift İKİ) ((çift SEKİZ) sıfır)))) ((çift ÜÇ) ((çift ON) sıfır)))
;; Performance:
(kaç-ö)
(kaç-s)

;; normalize: g-sayı g-sayı p-sayı -> p-sayı
;; Purpose: Take an unnormalized p sayı and normalize it by eleminating all digits which are larger than base
;; and doig carries.
(tanımla normalize
         (λ elde
           (λ taban
             (λ ç
               (eğer ((ve (sıfır? ç)) (sıfır? elde)) sıfır
                     ((((böl/mod (λ bölüm (λ kalan ((çift kalan) (((normalize bölüm)taban) (kuyruk-nv ç))))))
                        taban)sıfır)
                      ((topla elde) ((baş-nv sıfır) ç)))
                     )))))

;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

;; topla-p : g-sayı p-sayı p-sayı -> p-sayı
;; Purpose: To sum two p-sayı
(tanımla topla-p 
         (λ taban 
           (λ p1 
             (λ p2 
               (((normalize sıfır) taban) ((((ata-gelişmiş sıfır) topla) p1) p2))))))

;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

;; Test:
"topla-p"
((eşit-mi? ((p->g İKİ)(((topla-p İKİ)((g->p İKİ) ALTI)) ((g->p İKİ) DÖRT)))) ON)
(kaç-ö)
(kaç-s)
((eşit-mi? ((p->g İKİ)(((topla-p İKİ)((g->p İKİ) BİR)) ((g->p İKİ) ÜÇ)))) DÖRT)
(kaç-ö)
(kaç-s)
((eşit-mi? ((p->g İKİ)(((topla-p İKİ)((g->p İKİ) ALTI)) ((g->p İKİ) İKİ)))) SEKİZ)
(kaç-ö)
(kaç-s)

;; çarp-p-g : g-sayı psoitional-sayı g-sayı -> p-sayı
;; Purpose: To product a p sayı with a g-sayı.
(tanımla çarp-p-g 
         (λ taban 
           (λ p 
             (λ g 
               (((normalize sıfır) taban)((ata-basit (çarp g)) p))))))

;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

;; Test:
"çarp-p-g"
((çift-eşit-mi? (((çarp-p-g İKİ)((g->p İKİ) BİR)) DÖRT)) ((g->p İKİ) DÖRT))
(kaç-ö)
(kaç-s)
((eşit-mi? ((p->g İKİ)(((çarp-p-g İKİ)((g->p İKİ) ALTI)) DÖRT )))((çarp İKİ)((topla ON) İKİ)))
(kaç-ö)
(kaç-s)
((eşit-mi? ((p->g İKİ)(((çarp-p-g İKİ)((g->p İKİ) BİR)) İKİ ))) İKİ)
(kaç-ö)
(kaç-s)
((eşit-mi? ((p->g İKİ)(((çarp-p-g İKİ)((g->p İKİ) ALTI)) BİR ))) ALTI)
(kaç-ö)
(kaç-s)

;; çarp-p-p : g-sayı psoitional-sayı g-sayı -> p-sayı
;; Purpose: To product two p sayı
(tanımla çarp-p-p 
         (λ taban 
           (λ p1 
             (λ p2 
               (((katla 
                  (λ sol (λ sağ
                         (((topla-p taban)
                           (((çarp-p-g taban) p1) sol))
                          ((çift sıfır) sağ)))))
                 sıfır) p2)))))

;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

;; Test:
"çarp-p-p"
((eşit-mi? ((p->g İKİ)(((çarp-p-p İKİ)((g->p İKİ) ALTI)) ((g->p İKİ) DÖRT))))((çarp İKİ)((topla ON) İKİ)))
(kaç-ö)
(kaç-s)
((eşit-mi? ((p->g İKİ)(((çarp-p-p İKİ)((g->p İKİ) BİR)) ((g->p İKİ) ÜÇ)))) ÜÇ)
(kaç-ö)
(kaç-s)
((eşit-mi? ((p->g İKİ)(((çarp-p-p İKİ)((g->p İKİ) ALTI)) ((g->p İKİ) İKİ))))((topla ON) İKİ))
(kaç-ö)
(kaç-s)


;; çift-göster: çift -> g-sayı
;; Purpose: To show p sayı as g-sayı
(tanımla çift-göster
         (λ ç
           ((ata-basit göster) ç)))

;; Tests:
;(çift-göster(((topla-p İKİ)((g->p İKİ) BİR)) ((g->p İKİ) ÜÇ)))
;(çift-göster (((topla-p ON) ((çift BEŞ) ((çift ÜÇ) ((çift BİR) sıfır)))) ((çift ALTI) ((çift DOKUZ) sıfır))))
;(çift-göster(((çarp-p-g İKİ)((g->p İKİ) BİR)) İKİ ))
;(çift-göster (((çarp-p-p SEKİZ) ((çift DÖRT) ((çift ÜÇ) ((çift YEDİ) sıfır))))
;              ((çift ALTI) ((çift ON) ((çift İKİ) sıfır)))))
;(çift-göster(((çarp-p-p İKİ)((g->p İKİ) ALTI)) ((g->p İKİ) DÖRT)))
;(çift-göster(((çarp-p-p İKİ)((g->p İKİ) BİR)) ((g->p İKİ) ÜÇ)))

;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

;; taban-değiştir-p : g-sayı g-sayı -> (p-sayı -> g-sayı)
;; Purpose : To change base of p-sayı

(tanımla taban-değiştir-p
         (λ taban1
           (λ taban2
             (λ p
               (((katla 
                  (λ sol (λ sağ 
                           (((topla-p taban2) ((g->p taban2) sol)) 
                            (((çarp-p-g taban2) sağ) taban1))
                           ))) 
                 sıfır) p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTS & PERFORMANCE: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tests:
";;;;;;;;;;;;;;;;;"
(kaç-ö)
(kaç-s)

"EXTREME CASES:"

((çift-eşit-mi? (((taban-değiştir-p İKİ) ON) ((çift BİR) ((çift BİR)((çift BİR) sıfır))))) 
 ((çift (s (s (s (s (s (s (s sıfır)))))))) sıfır))

(kaç-ö)
(kaç-s)

((çift-eşit-mi? (((taban-değiştir-p İKİ) ÜÇ) ((çift BİR) ((çift BİR)((çift BİR) sıfır))))) 
 ((çift BİR)((çift İKİ) sıfır)))

(kaç-ö)
(kaç-s)

((çift-eşit-mi? (((taban-değiştir-p İKİ) BEŞ) ((çift BİR) ((çift BİR)((çift BİR) sıfır))))) 
 ((çift İKİ)((çift BİR) sıfır)))

(kaç-ö)
(kaç-s)

(((taban-değiştir-p ON) BEŞ) ((çift DOKUZ)((çift DOKUZ)((çift DOKUZ) sıfır))))
(kaç-ö)
(kaç-s)

(((taban-değiştir-p DOKUZ) BEŞ) ((çift DOKUZ)((çift DOKUZ)((çift DOKUZ) sıfır))))
(kaç-ö)
(kaç-s)

(((taban-değiştir-p SEKİZ) DÖRT) ((çift YEDİ)((çift YEDİ)((çift YEDİ) sıfır))))
(kaç-ö)
(kaç-s)

(((taban-değiştir-p SEKİZ) İKİ) ((çift DÖRT)((çift DÖRT)((çift DÖRT) sıfır))))
(kaç-ö)
(kaç-s)

(((taban-değiştir-p BEŞ) DOKUZ) ((çift DÖRT)((çift DÖRT)((çift DÖRT) sıfır))))
(kaç-ö)
(kaç-s)
"1000"
(((taban-değiştir-p ON) İKİ)((çift sıfır)((çift sıfır)((çift sıfır)((çift BİR) sıfır)))))
(kaç-ö)
(kaç-s)
"999"
(((taban-değiştir-p ON) İKİ)((çift DOKUZ)((çift DOKUZ)((çift DOKUZ) sıfır))))
(kaç-ö)
(kaç-s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Special Case Binary Arithmetic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ve-kapısı : g-number g-number -> g-number
;; Purpose : and-gate implementation for binary arithmetic
;; ASUMPTION : Function assumes it has given proper g-numbers at base two as input.

(tanımla ve-kapısı
         (λ önerme1
           (λ önerme2
             (eğer ((eşit-mi? önerme1) BİR)
                   önerme2
                   sıfır
                   ))))