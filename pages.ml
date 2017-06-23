(*
 * iocamlserver - IOCaml notebook server
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: HTML page templates
 *
 *)

(* see ipython/html/templates/page.html etc *)

(*

    Cow uses xmlm which is a XML codec.  We want to write HTML5.
    If you have something like;

    <script ...></script>

    cow will rewrite this to;

    <script .../>

    Thats quite proper XML but not HTML5 in which, according to the spec,
    the trailing '/' is silently dropped.  This buggers up the document
    structure as there is no end tag, unless its a so called void element
    in which case there should be no end tag at all.  In those cases the
    trailing '/' is ok.

    A quick and dirty fix is simply to include a space between the open
    and closing tags and xmlm will not rewrite it to a self closing tag.

    These are the void elements where we are OK.
        area, base, br, col, command, embed, hr, img, input,
        keygen, link, meta, param, source, track, wbr

    We have problems with these tags below and I've bodged the ones I
    could find.
         script, div and li

    A not unreasonable approach might be to rewrite this in XHTML at
    some point.

*)

let empty = {html| |html}

let page
    title
    base_project_url
    data_project
    data_base_project_url
    data_base_kernel_url
    data_notebook_id
    body_class
    stylesheet
    header
    site
    script =
  let vars =
    `O [ "title", `String title
       ; "base_project_url", `String base_project_url
       ; "data_project", `String data_project
       ; "data_base_project_url", `String data_base_project_url
       ; "data_base_kernel_url", `String data_base_kernel_url
       ; "data_notebook_id", `String data_notebook_id
       ; "body_class", `String body_class
       ; "stylesheet", `String stylesheet
       ; "header", `String header
       ; "site", `String site
       ; "script", `String script
       ] in
  let template = Mustache.of_string
  {html|

<html>
<head>
    <meta charset="utf-8"></meta>
    <title>{{title}}</title>
    <link rel="shortcut icon" type="image/x-icon" href=/static/base/images/favicon.ico />
    <meta http-equiv="X-UA-Compatible" content="chrome=1" />
    <link rel="stylesheet" href=/static/components/jquery-ui/themes/smoothness/jquery-ui.min.css type="text/css" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <!-- XXX or less -->
    {{{stylesheet}}}
    <link rel="stylesheet" href=/static/custom/custom.css type="text/css" />
    <script src=/static/components/requirejs/require.js type="text/javascript" charset="utf-8"> </script>

    <script>
      require.config({
          baseUrl: '/static/',
      });
    </script>

</head>

<body
    data-project={{data_project}}
    data-base-project-url={{data_base_project_url}}
    data-base-kernel-url={{data_base_kernel_url}}
    data-notebook-id={{data_notebook_id}}
    class={{body_class}}
>
<noscript>
    <div id='noscript'>
      IPython Notebook requires JavaScript.<br/>
      Please enable it to proceed.
  </div>
</noscript>

<div id="header" class="navbar navbar-static-top">
    <div class="navbar-inner navbar-nobg">
        <div class="container">
            <div id="ipython_notebook" class="nav brand pull-left">
              <a href="{{base_project_url}}" alt='dashboard'>
                    <img src="/static/base/images/ipynblogo.png"
                         alt='IPython Notebook'/>
                </a>
            </div>
            <!-- XXX login-widget -->
            {{header}}
        </div>
    </div>
</div>

<div id="site">
  {{{site}}}
</div>

<script src="/static/components/jquery/jquery.min.js"
        type="text/javascript" charset="utf-8"> </script>
<script src="/static/components/jquery-ui/ui/minified/jquery-ui.min.js"
        type="text/javascript" charset="utf-8"> </script>
<script src="/static/components/bootstrap/bootstrap/js/bootstrap.min.js"
        type="text/javascript" charset="utf-8"> </script>
<script src="/static/base/js/namespace.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/base/js/page.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/auth/js/loginwidget.js" type="text/javascript" charset="utf-8"> </script>

<!-- XXX if use_less -->
{{{script}}}
<script src="/static/custom/custom.js" type="text/javascript" charset="utf-8"> </script>

</body>

</html>
|html} in Mustache.render template vars

let notebook_stylesheet mathjax_url static_url =
  let mathjax_url' = mathjax_url ^ "?config=TeX-AMS_HTML-full&delayStartupUntil=configured" in
  let vars = `O ["mathjax_url", `String mathjax_url'] in
  let template = Mustache.of_string
  {html|

<script type="text/javascript"
        src={{mathjax_url}}
        charset="utf-8">
</script>

<script type="text/javascript">
// MathJax disabled, set as null to distingish from *missing* MathJax,
// where it will be undefined, and should prompt a dialog later.
window.mathjax_url = '{{mathjax_url}}';
</script>
<!--
<script type="text/javascript">
window.mathjax_url = '';
</script>
-->

<link rel="stylesheet" href="/static/components/codemirror/lib/codemirror.css" />
<link rel="stylesheet" href="/static/style/style.min.css" type="text/css"/>
<link rel="stylesheet" href="/static/notebook/css/override.css" type="text/css" />
|html} in Mustache.render template vars

let notebook_header =
  {html|
<span id="save_widget" class="nav pull-left">
    <span id="notebook_name"> </span>
    <span id="checkpoint_status"> </span>
    <span id="autosave_status"> </span>
</span>
|html}

let notebook_site online js_kernel =
  let file_menu =
    if online then
      {html|
        <li class="dropdown"><a href="#" class="dropdown-toggle" data-toggle="dropdown">File</a>
            <ul class="dropdown-menu">
              <li id="download_ipynb"><a href="#">Download Notebook (.ipynb)</a></li>
            </ul>
        </li>
      |html}
    else
      {html|
        <li class="dropdown"><a href="#" class="dropdown-toggle" data-toggle="dropdown">File</a>
            <ul class="dropdown-menu">
                <li id="new_notebook"><a href="#">New</a></li>
                <li id="open_notebook"><a href="#">Open...</a></li>
                <!-- <hr/> -->
                <li class="divider"> </li>
                <li id="copy_notebook"><a href="#">Make a Copy...</a></li>
                <li id="rename_notebook"><a href="#">Rename...</a></li>
                <li id="save_checkpoint"><a href="#">Save and Checkpoint</a></li>
                <!-- <hr/> -->
                <li class="divider"> </li>
                <li id="restore_checkpoint" class="dropdown-submenu"><a href="#">Revert to Checkpoint</a>
                  <ul class="dropdown-menu">
                    <li><a href="#"> </a></li>
                    <li><a href="#"> </a></li>
                    <li><a href="#"> </a></li>
                    <li><a href="#"> </a></li>
                    <li><a href="#"> </a></li>
                  </ul>
                </li>
                <li class="divider"> </li>
                <li class="dropdown-submenu"><a href="#">Download as</a>
                    <ul class="dropdown-menu">
                        <li id="download_ipynb"><a href="#">IPython (.ipynb)</a></li>
                        <li id="download_py"><a href="#">Python (.py)</a></li>
                    </ul>
                </li>
                <li class="divider"> </li>

                <li id="kill_and_exit"><a href="#" >Close and halt</a></li>
            </ul>
        </li>
      |html}
  in
  let kernel_menu =
    if js_kernel then
      {html| |html}
    else
      {html|
        <li class="dropdown"><a href="#" class="dropdown-toggle" data-toggle="dropdown">Kernel</a>
            <ul class="dropdown-menu">
                <li id="int_kernel"><a href="#">Interrupt</a></li>
                <li id="restart_kernel"><a href="#">Restart</a></li>
            </ul>
        </li>
      |html}
  in
  let vars =
    `O [ "file_menu", `String file_menu
       ; "kernel_menu", `String kernel_menu ] in
  let template = Mustache.of_string
  {html|
<div id="menubar-container" class="container">
<div id="menubar">
<div class="navbar">
  <div class="navbar-inner">
    <div class="container">
    <ul id="menus" class="nav">
        {{{file_menu}}}
        <li class="dropdown"><a href="#" class="dropdown-toggle" data-toggle="dropdown">Edit</a>
            <ul class="dropdown-menu">
                <li id="cut_cell"><a href="#">Cut Cell</a></li>
                <li id="copy_cell"><a href="#">Copy Cell</a></li>
                <li id="paste_cell_above" class="disabled"><a href="#">Paste Cell Above</a></li>
                <li id="paste_cell_below" class="disabled"><a href="#">Paste Cell Below</a></li>
                <li id="paste_cell_replace" class="disabled"><a href="#">Paste Cell &amp; Replace</a></li>
                <li id="delete_cell"><a href="#">Delete Cell</a></li>
                <li id="undelete_cell" class="disabled"><a href="#">Undo Delete Cell</a></li>
                <li class="divider"> </li>
                <li id="split_cell"><a href="#">Split Cell</a></li>
                <li id="merge_cell_above"><a href="#">Merge Cell Above</a></li>
                <li id="merge_cell_below"><a href="#">Merge Cell Below</a></li>
                <li class="divider"> </li>
                <li id="move_cell_up"><a href="#">Move Cell Up</a></li>
                <li id="move_cell_down"><a href="#">Move Cell Down</a></li>
                <li class="divider"> </li>
                <li id="select_previous"><a href="#">Select Previous Cell</a></li>
                <li id="select_next"><a href="#">Select Next Cell</a></li>
            </ul>
        </li>
        <li class="dropdown"><a href="#" class="dropdown-toggle" data-toggle="dropdown">View</a>
            <ul class="dropdown-menu">
                <li id="toggle_header"><a href="#">Toggle Header</a></li>
                <li id="toggle_toolbar"><a href="#">Toggle Toolbar</a></li>
            </ul>
        </li>
        <li class="dropdown"><a href="#" class="dropdown-toggle" data-toggle="dropdown">Insert</a>
            <ul class="dropdown-menu">
                <li id="insert_cell_above"><a href="#">Insert Cell Above</a></li>
                <li id="insert_cell_below"><a href="#">Insert Cell Below</a></li>
            </ul>
        </li>
        <li class="dropdown"><a href="#" class="dropdown-toggle" data-toggle="dropdown">Cell</a>
            <ul class="dropdown-menu">
                <li id="run_cell"><a href="#">Run</a></li>
                <li id="run_cell_in_place"><a href="#">Run in Place</a></li>
                <li id="run_all_cells"><a href="#">Run All</a></li>
                <li id="run_all_cells_above"><a href="#">Run All Above</a></li>
                <li id="run_all_cells_below"><a href="#">Run All Below</a></li>
                <li class="divider"> </li>
                <li id="change_cell_type" class="dropdown-submenu"><a href="#">Cell Type</a>
                    <ul class="dropdown-menu">
                      <li id="to_code"><a href="#">Code</a></li>
                      <li id="to_markdown"><a href="#">Markdown </a></li>
                      <li id="to_raw"><a href="#">Raw Text</a></li>
                      <li id="to_heading1"><a href="#">Heading 1</a></li>
                      <li id="to_heading2"><a href="#">Heading 2</a></li>
                      <li id="to_heading3"><a href="#">Heading 3</a></li>
                      <li id="to_heading4"><a href="#">Heading 4</a></li>
                      <li id="to_heading5"><a href="#">Heading 5</a></li>
                      <li id="to_heading6"><a href="#">Heading 6</a></li>
                    </ul>
                </li>
                <li class="divider"> </li>
                <li id="toggle_output"><a href="#">Toggle Current Output</a></li>
                <li id="all_outputs" class="dropdown-submenu"><a href="#">All Output</a>
                    <ul class="dropdown-menu">
                        <li id="expand_all_output"><a href="#">Expand</a></li>
                        <li id="scroll_all_output"><a href="#">Scroll Long</a></li>
                        <li id="collapse_all_output"><a href="#">Collapse</a></li>
                        <li id="clear_all_output"><a href="#">Clear</a></li>
                    </ul>
                </li>
            </ul>
        </li>
        {{{kernel_menu}}}
        <li class="dropdown"><a href="#" class="dropdown-toggle" data-toggle="dropdown">Help</a>
            <ul class="dropdown-menu">

                <li><a href="http://caml.inria.fr" target="_blank">OCaml</a></li>
                <li><a href="http://ocaml.org" target="_blank">OCaml Community</a></li>

                <li class="divider"> </li>
                <li><a href="http://opam.ocaml.org/" target="_blank">Opam Package Manager</a></li>
                <li><a href="https://github.com/ocaml/ocaml" target="_blank">OCaml On Github</a></li>
                <li><a href="http://caml.inria.fr/cgi-bin/viewvc.cgi/ocaml" target="_blank">OCaml SVN</a></li>

                <li class="divider"> </li>
                <li><a href="http://caml.inria.fr/pub/docs/manual-ocaml" target="_blank">OCaml Manual</a></li>
                <li><a href="http://realworldocaml.org" target="_blank">Real World OCaml</a></li>
                <li><a href="https://github.com/rizo/awesome-ocaml" target="_blank">Awesome OCaml!</a></li>

                <li class="divider"> </li>
                <li><a href="https://github.com/andrewray/iocaml" target="_blank">IOCaml Kernel On Github</a></li>
                <li><a href="https://github.com/andrewray/iocamlserver" target="_blank">IOCaml Server On Github</a></li>
                <li><a href="https://github.com/andrewray/iocamljs" target="_blank">IOCamlJS Kernel On Github</a></li>
                <li><a href="http://andrewray.github.io/iocamljs" target="_blank">IOCamlJS Demos</a></li>

                <li class="divider"> </li>
                <li><a href="http://ipython.org/documentation.html" target="_blank">IPython Help</a></li>
                <li><a href="http://ipython.org/ipython-doc/stable/interactive/notebook.html" target="_blank">Notebook Help</a></li>
                <li id="keyboard_shortcuts"><a href="#">Keyboard Shortcuts</a></li>

            </ul>
        </li>
    </ul>
    <div id="notification_area"> </div>
    </div>
  </div>
</div>
</div>
<div id="maintoolbar" class="navbar">
  <div class="toolbar-inner navbar-inner navbar-nobg">
    <div id="maintoolbar-container" class="container"> </div>
  </div>
</div>
</div>

<div id="ipython-main-app">

    <div id="notebook_panel">
        <div id="notebook"> </div>
        <div id="pager_splitter"> </div>
        <div id="pager">
            <div id='pager_button_area'>
            </div>
            <div id="pager-container" class="container"> </div>
        </div>
    </div>

</div>
<div id='tooltip' class='ipython_tooltip' style='display:none'> </div>
|html} in
  Mustache.render template vars

let notebook_scripts static_url kernel =
  let kernel =
    let kpath = "services/kernels/js/kernel" in
    static_url
      (match kernel with
       | `byte_code_kernel | `js_kernel_file(_) -> kpath ^ ".js"
       | `js_kernel(_, t) -> kpath ^ "." ^ t ^ ".js")
  in
  let vars = `O ["kernel", `String kernel] in
  let template = Mustache.of_string
    {html|
<script src="/static/components/codemirror/lib/codemirror.js" charset="utf-8"> </script>
<script type="text/javascript">
    CodeMirror.modeURL = '"/static/components/codemirror/mode/%N/%N.js"';
</script>
<script src="/static/components/codemirror/addon/mode/loadmode.js" charset="utf-8"> </script>
<script src="/static/components/codemirror/addon/mode/multiplex.js" charset="utf-8"> </script>
<script src="/static/components/codemirror/addon/mode/overlay.js" charset="utf-8"> </script>
<script src="/static/components/codemirror/addon/edit/matchbrackets.js" charset="utf-8"> </script>
<script src="/static/components/codemirror/addon/comment/comment.js" charset="utf-8"> </script>
<script src="/static/components/codemirror/mode/htmlmixed/htmlmixed.js" charset="utf-8"> </script>
<script src="/static/components/codemirror/mode/xml/xml.js" charset="utf-8"> </script>
<script src="/static/components/codemirror/mode/javascript/javascript.js" charset="utf-8"> </script>
<script src="/static/components/codemirror/mode/css/css.js" charset="utf-8"> </script>
<script src="/static/components/codemirror/mode/rst/rst.js" charset="utf-8"> </script>
<script src="/static/components/codemirror/mode/markdown/markdown.js" charset="utf-8"> </script>
<script src="/static/components/codemirror/mode/gfm/gfm.js" charset="utf-8"> </script>
<script src="/static/components/codemirror/mode/python/python.js" charset="utf-8"> </script>
<script src="/static/notebook/js/codemirror-ipython.js" charset="utf-8"> </script>

<script src="/static/components/highlight.js/build/highlight.pack.js" charset="utf-8"> </script>

<script src="/static/dateformat/date.format.js" charset="utf-8"> </script>

<script src="/static/base/js/events.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/base/js/utils.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/base/js/dialog.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/layoutmanager.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/mathjaxutils.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/outputarea.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/cell.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/celltoolbar.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/codecell.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/completer.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/textcell.js" type="text/javascript" charset="utf-8"> </script>
<script src={{{kernel}}} type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/savewidget.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/quickhelp.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/pager.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/menubar.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/toolbar.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/maintoolbar.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/notebook.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/notificationwidget.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/notificationarea.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/tooltip.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/config.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/main.js" type="text/javascript" charset="utf-8"> </script>

<script src="/static/notebook/js/contexthint.js" charset="utf-8"> </script>

<script src="/static/notebook/js/celltoolbarpresets/default.js" type="text/javascript" charset="utf-8"> </script>
<script src="/static/notebook/js/celltoolbarpresets/slideshow.js" type="text/javascript" charset="utf-8"> </script>
|html} in Mustache.render template vars

let dashboard_site path =
  let split_path =
    let open Re in (* requires path to start with a / *)
    let sl = char '/' in
    let path_comp = compile (seq [ sl; group (rep (compl [sl])) ]) in
    let rec f pos =
      try
        let x = exec ~pos path_comp path in
        get x 1 :: f (snd (get_ofs x 0))
      with Not_found -> []
    in
    f 0
  in
  let vars =
    `O ["split_path", `A (List.map (fun c -> `String c) split_path)] in
  let template = Mustache.of_string
  {html|

<div id="ipython-main-app" class="container">
  <div id="tabs" class="tabbable">
    <ul class="nav nav-tabs" id="tabs">
      <li class="active"><a href="#notebooks" data-toggle="tab">Notebooks</a></li>
      <!-- <li><a href="#clusters" data-toggle="tab">Clusters</a></li> -->
    </ul>

    <div class="tab-content">
      <div id="notebooks" class="tab-pane active">

        <div id="notebook_toolbar">
          <form id='alternate_upload'  class='alternate_upload' >
            <span id="drag_info" style="position:absolute" >
              To import a notebook, drag the file onto the listing below or <strong>click here</strong>.
            </span>
            <input type="file" name="datafile" class="fileinput" multiple='multiple' />
          </form>
          <span id="notebook_buttons">
            <button id="refresh_notebook_list" title="Refresh notebook list" class="btn btn-small">Refresh</button>
             <button id="new_notebook" title="Create new notebook" class="btn btn-small">New Notebook</button>
          </span>
        </div>

        <div id="notebook_list">
          <div id="notebook_list_header" class="row-fluid list_header">
            <div id="project_name">
              <ul class="breadcrumb">
                <li><span>/</span></li>
                {{#split_path}}
                <li>{{.}}<span>/</span></li>
                {{/split_path}}
              </ul>
            </div>
          </div>
        </div>

      </div>
    </div>

  </div>
</div>

|html} in Mustache.render template vars

let dashboard_stylesheet =
  {html|
<link rel="stylesheet" href="/static/style/style.min.css" type="text/css"/>
<link rel="stylesheet" href="/static/tree/css/override.css" type="text/css" />
|html}

let dashboard_scripts =
  {html|
    <script src="/static/base/js/dialog.js" type="text/javascript" charset="utf-8"> </script>
    <script src="/static/tree/js/notebooklist.js" type="text/javascript" charset="utf-8"> </script>
    <script src="/static/tree/js/clusterlist.js" type="text/javascript" charset="utf-8"> </script>
    <script src="/static/tree/js/main.js" type="text/javascript" charset="utf-8"> </script>
|html}

let generate_notebook_html ~base_path ~title ~path ~notebook_guid ~kernel =

  let static_url x = base_path ^ "/static/" ^ x in
  let mathjax_url = "https://cdn.mathjax.org/mathjax/latest/MathJax.js" in
  let base_project_url = if base_path="" then "/" else base_path in
  let data_base_project_url = base_path ^ "/" in
  let data_base_kernel_url = "/" in
  let body_class = "notebook_app" in

  let style = notebook_stylesheet mathjax_url static_url in
  let header = notebook_header in
  let site = notebook_site (base_path <> "") (kernel <> `byte_code_kernel) in
  let script = notebook_scripts static_url kernel in

  let page = page
      title base_project_url
      path data_base_project_url data_base_kernel_url
      notebook_guid body_class
      style header site script
  in
  "<!DOCTYPE HTML>\n" ^ page

let generate_dashboard_html ~path =
  let static_url x = "/static/" ^ x in
  let base_project_url = "/" in
  let data_base_project_url = "/" in
  let data_base_kernel_url = "/" in
  let body_class = "" in

  let style = dashboard_stylesheet static_url in
  let header = empty in
  let site = dashboard_site path in
  let script = dashboard_scripts static_url in

  let page = page
      "IOCaml Dashboard" base_project_url
      path data_base_project_url data_base_kernel_url
      "" body_class
      style header site script
  in
  "<!DOCTYPE HTML>\n" ^ page
