<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY % html "IGNORE">
<![%html;[
<!ENTITY % print "IGNORE">
<!ENTITY docbook.dsl PUBLIC
         "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN"
         CDATA dsssl>
]]>
<!ENTITY % print "INCLUDE">
<![%print;[
<!ENTITY docbook.dsl PUBLIC
         "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN"
         CDATA dsssl>
]]>
]>

<style-sheet>

<!--
;;******************************************************************************
;;******************************************************************************
;;                                PRINT
;;******************************************************************************
-->

<style-specification id="print" use="docbook">
<style-specification-body>

;; List of graphic filename extensions
(define %graphic-extensions%
'("pdf" "eps" "epsf" "gif" "tif" "tiff" "jpg" "jpeg" "png"))

;; Default graphic extension
(define %graphic-default-extension% "pdf")

;; Media preference
(define preferred-mediaobject-notations
 (list "PDF" "EPS" "PS" "JPG" "JPEG" "PNG" "linespecific"))

(define preferred-mediaobject-extensions
 (list "pdf" "eps" "ps" "jpg" "jpeg" "png"))

;;Tex Backend on
(define tex-backend #t)

;; Two-sided printing?
(define %two-side% #t)

;; Show URL links?
(define %show-ulinks% #t)

;; Enumerated sections?
(define %section-autolabel% #f)

;; Enumerated chapters?
(define %chapter-autolabel% #t)

;; Chapters and appendices have automatic labels?
(define %chap-app-running-head-autolabel% #t)

;; Line-spacing factor.
(define %line-spacing-factor% 1.1)

;; Generate book titlepage?
(define %generate-book-titlepage% #t)

;; Separate page for book titlepage?
(define %generate-book-titlepage-on-separate-page% #t)

;; Generate book TOC?
(define %generate-book-toc% #t)

;;What depth should the TOC generate?
;;!Only top level of appendixes!
(define (toc-depth nd)
  (if (string=? (gi nd) (normalize "book"))
      3
      (if (string=? (gi nd) (normalize "appendix"))
        0
        1)))

;; Generate TOC for parts?
(define %generate-part-toc% #f)

;; Things to appear on a book titlepage.
(define (book-titlepage-recto-elements)
  (list (normalize "mediaobject")
        (normalize "subtitle")
        (normalize "corpauthor")
        (normalize "authorgroup")
        (normalize "author")
        (normalize "orgname")
        (normalize "graphic")
        (normalize "copyright")
        (normalize "legalnotice")
        (normalize "releaseinfo")
        (normalize "publisher")
        (normalize "isbn")))

;; Separate page for part titlepage?
(define %generate-part-toc-on-titlepage% #f)

;; Generate part titlepage?
(define %generate-part-titlepage% #f)

;; Part intro on titlepage?
(define %generate-partintro-on-titlepage% #t)

;; List elements appearing in a LOT.
(define ($generate-book-lot-list$)
  (list (normalize "equation")))

;;******************************************************************************
;; Paper Size
;;******************************************************************************

(define %paper-type% "A4")

(define %page-width%
 (case %paper-type%
    (("A4") 210mm)
    (("USletter") 8.5in)
    (("USlandscape") 11in)))

(define %page-height%
 (case %paper-type%
    (("A4") 297mm)
    (("USletter") 11in)
    (("USlandscape") 8.5in)))

;;******************************************************************************
;; Quadding
;;******************************************************************************

(define %default-quadding% 'justify)
(define %component-title-quadding% 'start)
(define %section-title-quadding% 'start)
(define %section-subtitle-quadding% 'start)

;;******************************************************************************
;; Columns
;;******************************************************************************

(define %page-n-columns% 1)
(define %titlepage-n-columns% 1)

;;******************************************************************************
;; Formal Paragraphs
;;******************************************************************************

;; Run titles in paragraphs.
(element (formalpara title)
  (make sequence
  font-weight: 'bold
  ($runinhead$)))

</style-specification-body>
</style-specification>

<!--
;;******************************************************************************
;;******************************************************************************
;;                                HTML
;;******************************************************************************
-->

<style-specification id="html" use="docbook">
<style-specification-body>

;; This is necessary so that jade doesn't wreck symbolic entities.
;(declare-characteristic preserve-sdata?
;          "UNREGISTERED::James Clark//Characteristic::preserve-sdata?"
;          #f)

;;******************************************************************************
;; HTML header.
;;******************************************************************************

;; HTML header.
(define %html-pubid% "-//W3C//DTD HTML 4.01//EN")

;; Generate HTML 4.
(define %html40% #t)

;; Allow decoration by css.
;; (define %css-decoration% #t)
(define %css-decoration% #t)

;; META tags in HTML header.
(define %html-header-tags%
  '(("META" ("NAME" "robots") ("CONTENT" "noindex,nofollow"))))

;; Stylesheet name.
(define %stylesheet% "../tonto.css")

;; Stylesheet MIME type.
(define %stylesheet-type% "text/css")

;;******************************************************************************
;; Filenames.
;;******************************************************************************

;; Use "id" labels as filenames rather than useless numbers.
(define %use-id-as-filename% #t)

;; HTML file extension.
(define %html-ext% ".html")

(define %root-filename% "index")

;;******************************************************************************
;; Text.
;;******************************************************************************

;; Text alignment.
(define %default-quadding% "justify")

;;Show comment element?
(define %show-comments% #t)

;; Run in title in formal paragraphs.
(element formalpara
  (make element gi: "DIV"
        attributes: (list
                     (list "CLASS" (gi)))
        (make element gi: "P"
              (process-children))))
(element (formalpara title)
  (make element gi: "B"
        ($runinhead$)))

;; Captions after objects in the list
(define ($object-titles-after$)
  (list (normalize "figure")))

;; Indent Literal layouts?
(define %indent-literallayout-lines% #f)

;;Indent Programlistings? - No - might be confusing for input files.
(define %indent-programlisting-lines% #f)

;;Number lines in Programlistings?
(define %number-programlisting-lines% #f)

;;Should Variable lists be tables?
(define %always-format-variablelist-as-table%
 #f)

;;What is the length of the 'Term' in a variablelist?
(define %default-variablelist-termlength%
  20)

;;When true | If the terms are shorter than
;;the termlength above then the variablelist
;;will be formatted as a table.
(define %may-format-variablelist-as-table%
#f)

;;******************************************************************************
;; Colouring.
;;******************************************************************************

;; Shade verbatim items with a table?
(define %shade-verbatim% #t)

;; Verbatim attributes.
(define ($shade-verbatim-attr$)
  (list
   (list "BORDER" "0")
   (list "BGCOLOR" "#DDDDEE")
   (list "WIDTH" ($table-width$))))

;; Document body colours.
(define %body-attr%
  (list
   (list "BGCOLOR" "#FFFFFF")
   (list "TEXT" "#000000")
   (list "LINK" "#0000FF")
   (list "VLINK" "#840084")
   (list "ALINK" "#0000FF")))

;;******************************************************************************
;; Indexing.
;;******************************************************************************

(define (book-titlepage-separator side)
  (if (equal? side 'recto)
          (make empty-element gi: "IMG"
                attributes: (list
                             (list "SRC" "../hr.png")
                             (list "HEIGHT" "10")
                             (list "WIDTH" %gentext-nav-tblwidth%)))
          (make empty-element gi: "BR")))

;; Generate a TOC for books.
(define %generate-book-toc% #t)

;; Don't generate a TOC for chapters.
(define $generate-chapter-toc$
 (lambda () #f))

;; Generate a titlepage for books.
(define %generate-book-titlepage% #t)

;; Generate chapter numbering?
(define %chapter-autolabel% #f)

;; Generate section numbering?
(define %section-autolabel% #f)

;; Output middle names of authors?
(define %author-othername-in-middle% #t)

;; Things to appear on a book titlepage.
(define (book-titlepage-recto-elements)
  (list (normalize "mediaobject")
        (normalize "subtitle")
        (normalize "corpauthor")
        (normalize "authorgroup")
        (normalize "author")
        (normalize "orgname")
        (normalize "graphic")
        (normalize "copyright")
        (normalize "legalnotice")
        (normalize "releaseinfo")
        (normalize "publisher")
        (normalize "isbn")))

;; Reformat address.
  (element address
    (make sequence
      (make element gi: "DIV"
            attributes: (list (list "CLASS" (gi)))
            (process-children))))

;; Remove "mailto" links.
  (element email
    (make sequence
      (make element gi: "DIV"
            attributes: (list (list "CLASS" (gi)))
            (process-children))))

;;******************************************************************************
;; Tables.
;;******************************************************************************

;; Rewrite tgroup definition.
(element tgroup
  (let* ((wrapper   (parent (current-node)))
         (frameattr (attribute-string (normalize "frame") wrapper))
         (pgwide    (attribute-string (normalize "pgwide") wrapper))
         (footnotes (select-elements (descendants (current-node))
                                     (normalize "footnote")))
         (border (if (equal? frameattr (normalize "none"))
                     '(("BORDER" "0"))
                     '(("BORDER" "1"))))
         (bgcolor '(("BGCOLOR" "#E0E0E0")))
         (width (if (equal? pgwide "1")
                    (list (list "WIDTH" ($table-width$)))
                    '()))
         (head (select-elements (children (current-node)) (normalize "thead")))
         (body (select-elements (children (current-node)) (normalize "tbody")))
         (feet (select-elements (children (current-node)) (normalize "tfoot"))))
    (make element gi: "TABLE"
          attributes: (append
                       border
                       width
                       bgcolor
                       '(("CELLSPACING" "0"))
                       '(("CELLPADDING" "4"))
                       (if %cals-table-class%
                           (list (list "CLASS" %cals-table-class%))
                           '()))
          (process-node-list head)
          (process-node-list body)
          (process-node-list feet)
          (make-table-endnotes))))

;;******************************************************************************
;; Admon Graphics.
;;******************************************************************************

;; Use admon graphics?
(define %admon-graphics% #f)

;; Callouts as graphics?
(define %callout-graphics% #f)

;;******************************************************************************
;; Graphical Characters.
;;******************************************************************************

;; Change trademark entity for dodgy Netscape.
(element trademark
  (make sequence
    (process-children)
    (make element gi: "sup"
    (literal "TM"))))

;;******************************************************************************;; Chunks.
;;******************************************************************************
(define (chunk-element-list)
  (list (normalize "preface")
        (normalize "chapter")
        (normalize "appendix")
        (normalize "article")
        (normalize "glossary")
        (normalize "bibliography")
        (normalize "index")
        (normalize "colophon")
        (normalize "setindex")
        (normalize "reference")
        (normalize "refentry")
        (normalize "part")
        (normalize "sect1")
        (normalize "section")
        (normalize "book")
        (normalize "set")
        ))

;;******************************************************************************
;; Navigation.
;;******************************************************************************

;; Navigation in the header?
(define %header-navigation% #t)

;; Navigation in the bottom?
(define %footer-navigation% #t)

;; Use tables to align the navigation arrows?
(define %gentext-nav-use-tables% #t)

;; Width of navigation tables.
(define %gentext-nav-tblwidth% "100%")

;; Navigation arrows.
(define (gentext-en-nav-prev prev)
  (make sequence (literal " << Previous")))
(define (gentext-en-nav-next next)
  (make sequence (literal "Next >> ")))

;; Make sure using tables for navigation - tables can have a background colour
;; set easily, saves us using css.
(define %gentext-nav-use-ff% #f)

;; Redefine navigation header to allow background colour setting.
(define (default-header-nav-tbl-noff elemnode prev next prevsib nextsib)
  (let* ((r1? (nav-banner? elemnode))
         (r1-sosofo (make element gi: "TR"
                          (make element gi: "TH"
                                attributes: (list
                                             (list "COLSPAN" "3")
                                             (list "ALIGN" "center"))
                                (nav-banner elemnode))))
         (r2? (or (not (node-list-empty? prev))
                  (not (node-list-empty? next))
                  (nav-context? elemnode)))
         (r2-sosofo (make element gi: "TR"
                          (make element gi: "TD"
                                attributes: (list
                                             (list "WIDTH" "33%")
                                             (list "ALIGN" "left")
                                             (list "VALIGN" "bottom"))
                                (if (node-list-empty? prev)
                                    (make entity-ref name: "nbsp")
                                    (make element gi: "A"
                                          attributes: (list
                                                       (list "HREF"
                                                             (href-to
                                                              prev))
                                                       (list "ACCESSKEY"
                                                             "P"))
                                          (gentext-nav-prev prev))))
                          (make element gi: "TD"
                                attributes: (list
                                             (list "WIDTH" "33%")
                                             (list "ALIGN" "center")
                                             (list "BGCOLOR" "#DDDDEE")
                                             (list "VALIGN" "bottom"))
                                   (make entity-ref name: "nbsp"))
                          (make element gi: "TD"
                                attributes: (list
                                             (list "WIDTH" "33%")
                                             (list "ALIGN" "right")
                                             (list "VALIGN" "bottom"))
                                (if (node-list-empty? next)
                                    (make entity-ref name: "nbsp")
                                    (make element gi: "A"
                                          attributes: (list
                                                       (list "HREF"
                                                             (href-to
                                                              next))
                                                       (list "ACCESSKEY"
                                                             "N"))
                                          (gentext-nav-next next)))))))
    (if (or r1? r2?)
        (make element gi: "DIV"
              attributes: '(("CLASS" "NAVHEADER"))
          (make element gi: "TABLE"
                attributes: (list
                             (list "SUMMARY" "Header navigation table")
                             (list "WIDTH" %gentext-nav-tblwidth%)
                             (list "BORDER" "0")
                             (list "CELLPADDING" "0")
                             (list "CELLSPACING" "10")
                             (list "BGCOLOR" "#DDDDEE"))
                (if r1? r1-sosofo (empty-sosofo))
                (if r2? r2-sosofo (empty-sosofo)))
          (make empty-element gi: "IMG"
                attributes: (list
                             (list "SRC" "../hr.png")
                             (list "HEIGHT" "10")
                             (list "WIDTH" %gentext-nav-tblwidth%))))
        (empty-sosofo))))

;; Redefine navigation footer to allow background colour setting.
(define (default-footer-nav-tbl elemnode prev next prevsib nextsib)
  (let ((r1? (or (not (node-list-empty? prev))
                 (not (node-list-empty? next))
                 (nav-home? elemnode)))
        (r2? (or (not (node-list-empty? prev))
                 (not (node-list-empty? next))
                 (nav-up? elemnode)))

        (r1-sosofo (make element gi: "TR"
                         (make element gi: "TD"
                               attributes: (list
                                            (list "WIDTH" "33%")
                                            (list "ALIGN" "left")
                                            (list "VALIGN" "top"))
                               (if (node-list-empty? prev)
                                   (make entity-ref name: "nbsp")
                                   (make element gi: "A"
                                         attributes: (list
                                                      (list "HREF" (href-to
                                                                    prev))
                                                      (list "ACCESSKEY"
                                                            "P"))
                                         (gentext-nav-prev prev))))
                         (make element gi: "TD"
                               attributes: (list
                                            (list "WIDTH" "34%")
                                            (list "ALIGN" "center")
                                            (list "VALIGN" "top"))
                               (nav-home-link elemnode))
                         (make element gi: "TD"
                               attributes: (list
                                            (list "WIDTH" "33%")
                                            (list "ALIGN" "right")
                                            (list "VALIGN" "top"))
                               (if (node-list-empty? next)
                                   (make entity-ref name: "nbsp")
                                   (make element gi: "A"
                                         attributes: (list
                                                      (list "HREF" (href-to
                                                                    next))
                                                      (list "ACCESSKEY"
                                                            "N"))
                                         (gentext-nav-next next))))))
        (r2-sosofo (make element gi: "TR"
                         (make element gi: "TD"
                               attributes: (list
                                            (list "WIDTH" "33%")
                                            (list "ALIGN" "JUSTIFY")
                                            (list "VALIGN" "top"))
                               (if (node-list-empty? prev)
                                   (make entity-ref name: "nbsp")
                                   (element-title-sosofo prev)))
                         (make element gi: "TD"
                               attributes: (list
                                            (list "WIDTH" "34%")
                                            (list "ALIGN" "center")
                                            (list "BGCOLOR" "#DDDDEE")
                                            (list "VALIGN" "top"))
                               (if (nav-up? elemnode)
                                   (nav-up elemnode)
                                   (make entity-ref name: "nbsp")))
                         (make element gi: "TD"
                               attributes: (list
                                            (list "WIDTH" "33%")
                                            (list "ALIGN" "JUSTIFY")
                                            (list "VALIGN" "top"))
                               (if (node-list-empty? next)
                                   (make entity-ref name: "nbsp")
                                   (element-title-sosofo next))))))
    (if (or r1? r2?)
        (make element gi: "DIV"
              attributes: '(("CLASS" "NAVFOOTER"))
          (make empty-element gi: "BR")
          (make empty-element gi: "IMG"
                attributes: (list
                             (list "SRC" "../hr.png")
                             (list "HEIGHT" "10")
                             (list "WIDTH" %gentext-nav-tblwidth%)))
;;          (make empty-element gi: "HR"
;;                attributes: (list
;;                             (list "ALIGN" "LEFT")
;;                             (list "WIDTH" %gentext-nav-tblwidth%)))
          (make element gi: "TABLE"
                attributes: (list
                             (list "SUMMARY" "Footer navigation table")
                             (list "WIDTH" %gentext-nav-tblwidth%)
                             (list "BORDER" "0")
                             (list "CELLPADDING" "0")
                             (list "CELLSPACING" "10")
                             (list "BGCOLOR" "#DDDDEE"))
                (if r1? r1-sosofo (empty-sosofo))
                (if r2? r2-sosofo (empty-sosofo))))
        (empty-sosofo))))

</style-specification-body>
</style-specification>

<external-specification id="docbook" document="docbook.dsl">
</style-sheet>
