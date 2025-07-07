CLASS zcl_html_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS:
      get_html_content_code
        IMPORTING iv_object_name  TYPE string
                  iv_diagram_type TYPE string
                  iv_mermaid_code TYPE string
        RETURNING VALUE(rv_html)  TYPE string,

      get_html_content_error
        IMPORTING iv_object_name  TYPE string
                  iv_diagram_type TYPE string
                  iv_error_msg    TYPE string
        RETURNING VALUE(rv_html)  TYPE string,

      get_html_content_diagram
        IMPORTING iv_object_name  TYPE string
                  iv_diagram_type TYPE string
                  iv_mermaid_code TYPE string
        RETURNING VALUE(rv_html)  TYPE string.

  PRIVATE SECTION.

    CLASS-METHODS:
      get_diagram_prefix
        IMPORTING iv_diagram_type  TYPE string
        RETURNING VALUE(rv_prefix) TYPE string,

      js_draw_diagram
        RETURNING VALUE(rv_js) TYPE string,

      js_copy_function
        IMPORTING iv_mermaid_code TYPE string
        RETURNING VALUE(rv_js) TYPE string,

      js_download_function
        RETURNING VALUE(rv_js) TYPE string.

ENDCLASS.


CLASS zcl_html_generator IMPLEMENTATION.

  METHOD get_html_content_code.
    DATA(lv_js_copy) = js_copy_function( iv_mermaid_code ).

    rv_html =
            |<h1>{ get_diagram_prefix( iv_diagram_type ) } Diagram for { iv_object_name }</h1>| &&
            |<div class="export-container">| &&
            |  <button id="copyButton">Copy Diagram Code</button>| &&
            |</div>| &&
            |<p>Paste this code at <a href="https://mermaid.live/edit">https://mermaid.live/edit</a> to view a preview of your diagram:</p>| &&
            |<pre id="mermaidCode">{ iv_mermaid_code }</pre>| &&
            lv_js_copy.
  ENDMETHOD.

  METHOD get_html_content_error.
    rv_html =
            |<h1>Error generating { get_diagram_prefix( iv_diagram_type ) } Diagram for { iv_object_name }</h1>| &&
            |<p>Unfortunately there has been an error generating your diagram. Please make sure your file does not include any syntax errors.</p>| &&
            |<h2>Error Message</h2>| &&
            |<p>{ iv_error_msg }</p>|.
  ENDMETHOD.

  METHOD get_diagram_prefix.
    IF iv_diagram_type = 'CLASS' OR iv_diagram_type = 'INTF'.
      rv_prefix = 'Class '.
    ELSEIF iv_diagram_type = 'CDS' OR iv_diagram_type = 'VIEW'.
      rv_prefix = 'Entity-Relationship '.
    ELSEIF iv_diagram_type = 'BDEF'.
      rv_prefix = 'Behavior-Definition '.
    ELSE.
      rv_prefix = ''.
    ENDIF.
  ENDMETHOD.

  METHOD get_html_content_diagram.
    DATA(lv_prefix) = get_diagram_prefix( iv_diagram_type ).
    DATA(lv_js_draw) = js_draw_diagram( ).
    DATA(lv_js_copy) = js_copy_function( iv_mermaid_code ).
    DATA(lv_js_download) = js_download_function( ).

    " Create graphDefinition string
    DATA(lv_graph_def) =
      |---\n| &&
      |config:\n| &&
      |   theme: dark\n| &&
      |---\n| &&
      |{ iv_mermaid_code }\n|.

    rv_html =
      |<style>| &&
      |  html, body \{ height:100%; width:100%; margin:0; padding:0; overflow: hidden\}| &&
      |  #graphDiv \{ height:90vh; width:100%; overflow:auto; margin: auto; cursor: grab;\}| &&
      |  svg \{height: 100%; max-width: 100% \}| &&
      |</style>| &&
      |<h1>{ lv_prefix } Diagram for { iv_object_name }</h1>| &&
      |<div class="export-container">| &&
      |  <button id="downloadButton">Download SVG</button>| &&
      |  <button id="copyButton">Copy Diagram Code</button>| &&
      |</div>| &&
      |<div id="graphDiv"></div>| &&
      |<script src="https://bumbu.me/svg-pan-zoom/dist/svg-pan-zoom.js"></script>| &&
      |<script type="module">| &&
      |  import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';| &&
      |  mermaid.initialize(\{ startOnLoad: false \});| &&
      |  const graphDefinition = `{ lv_graph_def }`;\n| &&
      lv_js_draw &&
      lv_js_copy &&
      lv_js_download &&
      |</script>|.
  ENDMETHOD.


  METHOD js_draw_diagram.
    rv_js =
      |  const drawDiagram = async () => \{| &&
      |    const element = document.querySelector('#graphDiv');| &&
      |    const \{ svg \} = await mermaid.render('mySvgId', graphDefinition);| &&
      |    window.latestMermaidSvg = svg;| &&
      |    element.innerHTML = svg;| &&
      |    window.panZoomTiger = svgPanZoom('#mySvgId', \{| &&
      |      zoomEnabled: true,| &&
      |      viewportSelector: 'test',|  &&
      |      contain: true,| &&
      |      center: true| &&
      |    \});| &&
      |    document.getElementById('mySvgId').style.maxWidth = null;| &&
      |  \};| &&
      |  await drawDiagram();|.
  ENDMETHOD.

  METHOD js_copy_function.
    rv_js =
      |  document.getElementById('copyButton').addEventListener('click', () => \{| &&
      |    const textarea = document.createElement("textarea");| &&
      |    textarea.value = `{ iv_mermaid_code }`;| &&
      |    textarea.setAttribute("readonly", "");| &&
      |    textarea.style.position = "absolute";| &&
      |    textarea.style.left = "-9999px";| &&
      |    document.body.appendChild(textarea);| &&
      |    textarea.select();| &&
      |    try \{| &&
      |      document.execCommand("copy");| &&
      |      alert("Diagram code copied to clipboard!");| &&
      |    \} catch (err) \{| &&
      |      console.error("Copy failed:", err);| &&
      |      alert("Copy failed.");| &&
      |    \}| &&
      |    document.body.removeChild(textarea);| &&
      |  \});|.
  ENDMETHOD.

  METHOD js_download_function.
    rv_js =
      |  document.getElementById('downloadButton').addEventListener('click', () => \{| &&
      |    const blob = new Blob([window.latestMermaidSvg], \{ type: "image/svg+xml;charset=utf-8" \});| &&
      |    const url = URL.createObjectURL(blob);| &&
      |    const link = document.createElement('a');| &&
      |    link.href = url;| &&
      |    link.download = 'diagram.svg';| &&
      |    document.body.appendChild(link);| &&
      |    link.click();| &&
      |    document.body.removeChild(link);| &&
      |    URL.revokeObjectURL(url);| &&
      |  \});|.
  ENDMETHOD.

ENDCLASS.

