#lang at-exp racket
(require scribble/manual
         (for-label racket/base
                    scribble/core
                    scribble/base
                    scribble/html-properties
                    scribble/latex-properties))

(provide (all-defined-out))

(define rkt_element @racket[element?])

(define rkt_part @racket[part?])

(define rkt_block @racket[block?])

(define rkt_style @racket[style?])

(define rkt_item @racket[item?])

(define rkt_collect_info @racket[collect-info?])
(define rkt_resolve_info @racket[resolve-info?])

(define rkt_paragraph @racket[paragraph?])
(define rkt_nested_flow @racket[nested-flow?])
(define rkt_compound_paragraph @racket[compound-paragraph?])
(define rkt_table @racket[table?])
(define rkt_itemization @racket[itemization?])
(define rkt_traverse_block @racket[traverse-block?])
(define rkt_delayed_block @racket[delayed-block?])

(define rkt_multarg_element @racket[multiarg-element?])
(define rkt_traverse_element @racket[traverse-element?])
(define rkt_part_relative_element @racket[part-relative-element?])
(define rkt_collect_element @racket[collect-element?])
(define rkt_delayed_element @racket[delayed-element?])
(define rkt_render_element @racket[render-element?])

(define rkt_secref @racket[secref])

(define rkt_read @racket[read])

(define rkt_numberer @racket[numberer?])
(define rkt_document_version @racket[document-version?])
(define rkt_document_date @racket[document-date?])
(define rkt_document_source @racket[document-source?])
(define rkt_color @racket[color-property?])
(define rkt_background_color @racket[background-color-property?])
(define rkt_cell_padding_property @racket[cell-padding-property?])
(define rkt_render_convertible_as @racket[render-convertible-as?])
(define rkt_link_render_style @racket[link-render-style?])
(define rkt_box_mode @racket[box-mode?])
(define rkt_target_url @racket[target-url?])
(define rkt_table_columns @racket[table-columns?])
(define rkt_table_cells @racket[table-cells?])

(define rkt_body_id @racket[body-id?])
(define rkt_alt_tag @racket[alt-tag?])
(define rkt_attributes_id @racket[attributes?])
(define rkt_column_attributes_id @racket[column-attributes?])
(define rkt_head_extra @racket[head-extra?])
(define rkt_head_addition @racket[head-addition?])
(define rkt_hover_property @racket[hover-property?])
(define rkt_part_title_and_content_wrapper @racket[part-title-and-content-wrapper])
(define rkt_part_link_redirect @racket[part-link-redirect?])
(define rkt_url_anchor @racket[url-anchor?])
(define rkt_script_property @racket[script-property?])
(define rkt_xexpr_property @racket[xexpr-property?])
(define rkt_link_resource @racket[link-resource?])
(define rkt_install_resource @racket[install-resource?])

(define rkt_command_extras @racket[command-extras?])
