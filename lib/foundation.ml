
(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Cow.Html

module Link = struct
  type t = string * Uri.t
  type links = t list
  let link ?(cl="") (txt, uri) = a ~href:uri ~cls:cl (string txt)

  let mk_ul_links ~cl ~links = ul ~cls:cl links

  let top_nav ?(align=`Right) (links:links) =
    let links = List.map (link ~cl:"") links in
    let cl = match align with `Right -> "right" | `Left -> "left" in
    mk_ul_links ~cl ~links

  let button_group (links:links) =
    let links = List.map (link ~cl:"button") links in
    mk_ul_links ~cl:"button-group" ~links

  let side_nav (links:links) =
    let links = List.map (link ~cl:"") links in
    mk_ul_links ~cl:"" ~links

  let bottom_nav (links:links) =
    let links = List.map (link ~cl:"") links in
    mk_ul_links ~cl:"inline-list right" ~links
end

module Sidebar = struct
  type t = [
    | `link of Link.t
    | `active_link of Link.t
    | `divider
    | `text of string
    | `html of Cow.Xml.t
  ]

  let t ~title ~content =
    let to_html = function
      |`link l        -> li (Link.link l)
      |`active_link l -> li ~cls:"active" (Link.link l)
      |`divider       -> li ~cls:"divider" empty
      |`html h        -> li h
      |`text t        -> li (string t)
    in
    let rec make = function
      | []     -> []
      | hd::tl -> to_html hd :: make tl
    in
    h5 (string title)
    ++ ul ~add_li:false ~cls:"side-nav" (make content)
end

module Index = struct
  let t ~top_nav =
    top_nav
    ++ br
    ++ div ~cls:"grid-x" (
      div ~cls:"cell large-12" (
        img (Uri.of_string "http://placehold.it/1000x400&amp;text=img")
        ++ hr
      ))
end

let rec intercalate x = function
  | []    -> []
  | [e]   -> [e]
  | e::es -> e :: x :: intercalate x es

module Blog = struct
 
  let post ~title ~authors ~date ~image ~tags ~content =
    let open Link in
    let author = match authors with
      | [] -> empty
      | _  ->
        let a_nodes =
          intercalate (string ", ") (List.map (link ~cl:"") authors)
        in
        string "By " ++ list a_nodes
    in
    let tag_block = match tags with
      | None -> empty
      | Some t ->
        let t_nodes =
          List.map (fun f -> a ~cls:"tag-cloud-individual-tag" (string f)) t
        in
        string "" ++ list t_nodes
    in
    let title_text, title_uri = title in
    div ~cls:"responsive-blog-post" (
    div ~cls:"individual-post" (
      date
      ++ h4 (a ~href:title_uri (string title_text))
      ++ (match image with
          | None -> []
          | Some uri -> img uri)
      ++ p (i author)
      ++ content
      ++ div ~cls:"tag-cloud-section" (
        h5 ~cls:"tag-cloud-title" (string "Tags")
        ++ div ~cls:"tag-cloud" (
          tag_block
        )
      )
    ))

  let t ~title ~subtitle ~sidebar ~posts ~copyright ?pages () =
    let page_links = match pages with
      | None -> empty
      | Some p -> (
          let previous_link = if (fst p) = 1 then
              li ~cls:"pagination-previous disabled" (string "Previous")
            else li ~cls:"pagination-previous"
                ~attrs:["aria-label", "Previous page"]
                (a ~href:(Uri.of_string ("/blog/" ^ (string_of_int ((fst p) - 1))))
                   (string "Previous"))
          in
          let next_link = if (fst p) = (snd p) then
              li ~cls:"pagination-next disabled" (string "Next")
            else li ~cls:"pagination-next"
                ~attrs:["aria-label", "Next page"]
                (a ~href:(Uri.of_string ("/blog/" ^ (string_of_int ((fst p) + 1))))
                   (string "Next"))
          in
          let rec build_list acc pg =
            match pg with
            | 0 -> acc
            | x -> (
                let lnk = match (x = (fst p)) with
                  | true -> li ~cls:"current" (string (string_of_int x))
                  | false -> li ~attrs:["aria-label", "Page " ^ (string_of_int x)]
                               (a ~href:(Uri.of_string ("/blog/" ^ (string_of_int x)))
                                  (string (string_of_int x)))
                in
                build_list (lnk :: acc) (x-1))
          in
          let link_list = previous_link :: build_list [next_link] (snd p) in
          div ~cls:"grid-x align-center" (
        nav ~attrs:["aria-label", "Pagination"]
          (ul ~add_li:false ~cls:"pagination text-center"
             link_list
          ))
        )
    in
    let subtitle =
      match subtitle with
      | None   -> empty
      | Some s -> small (string s)
    in
    list [
      div ~cls:"grid-x"
        (div ~cls:"cell large-9" (h2 (string title ++ subtitle)));
      div ~cls:"grid-x grid-margin-x" (
        div ~cls:"cell large-7 large-offset-2" ~attrs:["role", "content"] posts
        ++ aside ~cls:"cell large-2 callout secondary" sidebar
      );
      page_links;
      footer ~cls:"grid-x" (
        div ~cls:"cell" (
          hr
          ++ div ~cls:"grid-x" (
            div ~cls:"cell large-6" (
              p (small (string "&copy; Copyright " ++ copyright))
            ))))
    ]

end

let body ?google_analytics ?highlight ~title:t ~headers ~content ~trailers () =
  (* Cannot be inlined below as the $ is interpreted as an
     antiquotation *)
  let js_init = [`Data "$(document).foundation();"] in
  let highlight_css, highlight_trailer = match highlight with
    | None       -> empty, empty
    | Some style ->
      link ~rel:"stylesheet" (Uri.of_string style),
      script ~src:(Uri.of_string "/js/vendor/highlight.pack.js") empty
      ++ script (string "hljs.initHighlightingOnLoad(); ")
  in
  let ga =
    match google_analytics with
    | None        -> []
    | Some (a, d) ->
      script ~ty:"text/javascript" (
        string @@ Printf.sprintf
          "//<![CDATA[\n\
           var _gaq = _gaq || [];\n\
           _gaq.push(['_setAccount', '%s']);\n\
           _gaq.push(['_setDomainName', '%s']);\n\
           _gaq.push(['_trackPageview']);\n\
           \n\
           (function() {\n\
          \  var ga = document.createElement('script'); \
          \    ga.type = 'text/javascript'; \
          \    ga.async = true;\n\
          \  ga.src = ('https:' == document.location.protocol\
          \    ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';\n\
          \  var s = document.getElementsByTagName('script')[0]; \
          \    s.parentNode.insertBefore(ga, s);\n\
           })();\n\
           //]]>" a d)
  in
  head (list [
      meta ["charset","utf-8"];
      meta ["name","viewport"; "content","width=device-width"];
      title (string t);
      link ~rel:"stylesheet" (Uri.of_string "/css/foundation.min.css");
      link ~rel:"stylesheet" (Uri.of_string "/css/site.css");
      highlight_css;
      ga;
      headers;
    ])
  ++ body (list [
      content;
      script ~src:(Uri.of_string "/js/vendor/jquery.js") empty;
      script ~src:(Uri.of_string "/js/vendor/foundation.min.js") empty;
      script js_init;
      highlight_trailer;
      trailers
    ])

let top_nav ~title ~title_uri ~nav_links =
  div ~cls:"top-bar" (
      div ~cls:"top-bar-left" (
        ul ~add_li:false ~cls:"dropdown menu" ~attrs: ["data-dropdown-menu", ""][
          li ~cls:"menu-text" (a ~href:title_uri title);
          nav_links;
      ]
      )
    )

let page ~body =
  Printf.sprintf
    "<!DOCTYPE html>\n\
    \  <!--[if IE 8]><html class=\"no-js lt-ie9\" lang=\"en\" \
     xmlns=\"http://www.w3.org/1999/xhtml\"><![endif]-->\n\
    \  <!--[if gt IE 8]><!--><html class=\"no-js\" lang=\"en\" \
     xmlns=\"http://www.w3.org/1999/xhtml\"><!--<![endif]-->\n\
     %s\n\
     </html>" (Cow.Html.to_string body)
