CLASS zcl_cds_extractor DEFINITION
  INHERITING FROM zcl_extractor_utilities
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " Type definitions for CDS metadata structure
    TYPES: BEGIN OF ty_field,
             field_name  TYPE string,
             key_field   TYPE abap_bool,
             foreign_key TYPE abap_bool,
             association TYPE string,
             key_source  TYPE string, " Source of key detection
           END OF ty_field.

    TYPES tt_fields TYPE STANDARD TABLE OF ty_field WITH EMPTY KEY.

    " Enhanced association structure to track relationship levels
    TYPES: BEGIN OF ty_association_info,
             entity_name    TYPE string,
             relationship   TYPE string, " 'ASSOCIATION' or 'COMPOSITION'
             source_entity  TYPE string, " Entity that has this association
             discovery_level TYPE i,     " Level at which this was discovered
           END OF ty_association_info.

    TYPES tt_association_info TYPE STANDARD TABLE OF ty_association_info WITH EMPTY KEY.

    TYPES: BEGIN OF ty_cds_structure,
             view_name         TYPE string,         " CDS view name
             description       TYPE string,         " View description
             fields            TYPE tt_fields,      " table of fields
             associations      TYPE string_table,   " Associated entities
             compositions      TYPE string_table,   " Composition relationships
             enhanced_associations TYPE tt_association_info, " Enhanced association info
             has_errors        TYPE abap_bool,      " Error flag
             error_info        TYPE ty_error_info,  " Error details
           END OF ty_cds_structure.

    " Enhanced entity collection with discovery metadata
    TYPES: BEGIN OF ty_entity_collection,
             main_entity         TYPE ty_cds_structure,
             related_entities    TYPE STANDARD TABLE OF ty_cds_structure WITH EMPTY KEY,
             all_associations    TYPE tt_association_info, " All discovered associations
             discovery_depth     TYPE i,                   " Actual depth reached
             total_entities      TYPE i,                   " Total entities discovered
           END OF ty_entity_collection.

    " Extract CDS view metadata with comprehensive error handling
    METHODS: extract_cds_metadata
      IMPORTING iv_cds_name         TYPE string
                iv_validate_access  TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(rs_structure) TYPE ty_cds_structure,

      " Extract CDS metadata including related entities with recursive association discovery
      extract_cds_with_related
        IMPORTING iv_cds_name         TYPE string
                  iv_depth_level      TYPE i DEFAULT 2
                  iv_max_entities     TYPE i DEFAULT 50
        RETURNING VALUE(rs_collection) TYPE ty_entity_collection,

      " Validate CDS view exists and is accessible
      validate_cds_view
        IMPORTING iv_cds_name     TYPE string
        RETURNING VALUE(rv_valid) TYPE abap_bool.

    INTERFACES zif_extractor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    " Extract field information using XCO framework
    METHODS: get_cds_fields_xco
      IMPORTING iv_cds_name      TYPE string
      RETURNING VALUE(rt_fields) TYPE tt_fields
      RAISING   cx_root,

      " Extract associations using XCO with enhanced information
      get_cds_associations_enhanced
        IMPORTING iv_cds_name               TYPE string
                  iv_source_entity          TYPE string
                  iv_discovery_level        TYPE i DEFAULT 0
        RETURNING VALUE(rt_associations)    TYPE tt_association_info,

      " Extract associations using XCO (legacy method)
      get_cds_associations_xco
        IMPORTING iv_cds_name            TYPE string
        RETURNING VALUE(rt_associations) TYPE string_table,

      " Extract compositions using XCO (legacy method)
      get_cds_compositions_xco
        IMPORTING iv_cds_name            TYPE string
        RETURNING VALUE(rt_compositions) TYPE string_table,

      " Recursively discover all associations
      discover_associations_recur
        IMPORTING iv_start_entity       TYPE string
                  iv_max_depth          TYPE i DEFAULT 2
                  iv_max_entities       TYPE i DEFAULT 50
        RETURNING VALUE(rt_all_associations) TYPE tt_association_info,

      " Check if entity was already processed to avoid infinite loops
      is_entity_processed
        IMPORTING iv_entity_name TYPE string
                  it_processed   TYPE string_table
        RETURNING VALUE(rv_processed) TYPE abap_bool,

      " Check if association already exists in collection
      is_association_exists
        IMPORTING iv_entity_name      TYPE string
                  it_associations     TYPE tt_association_info
        RETURNING VALUE(rv_exists)    TYPE abap_bool,

      " Get unique entities from association collection
      get_unique_entities
        IMPORTING it_associations     TYPE tt_association_info
        RETURNING VALUE(rt_entities)  TYPE string_table,

      " Check if object is a CDS view or database table
      get_object_type
        IMPORTING iv_object_name TYPE string
        RETURNING VALUE(rv_type) TYPE string.

ENDCLASS.

CLASS zcl_cds_extractor IMPLEMENTATION.

  METHOD zif_extractor~generate_mermaid_code.
    " Extract main CDS with related entities using enhanced recursive discovery
    DATA(ls_collection) = extract_cds_with_related(
      iv_cds_name = iv_object_name
      iv_depth_level = 3  " Increased depth for better discovery
      iv_max_entities = 100 " Allow more entities
    ).

    " Check if main structure has errors
    IF ls_collection-main_entity-has_errors = abap_true.
      rs_result-success = abap_false.
      rs_result-message = ls_collection-main_entity-error_info-error_message.
      RETURN.
    ENDIF.

    " Generate Mermaid ER diagram syntax
    DATA(lv_mermaid) = |erDiagram\n|.

    " Validate main entity has fields
    IF lines( ls_collection-main_entity-fields ) = 0.
      rs_result-success = abap_false.
      rs_result-message = |No fields found for main entity.|.
      RETURN.
    ENDIF.

    " First, define all entities (main + related)
    " Main entity
    lv_mermaid = lv_mermaid && |    { ls_collection-main_entity-view_name } \{\n|.
    LOOP AT ls_collection-main_entity-fields INTO DATA(ls_field) WHERE field_name IS NOT INITIAL.
      DATA(lv_key_indicator) = COND string( WHEN ls_field-key_field = abap_true THEN 'KEY' ELSE 'x' ).
      lv_mermaid = |{ lv_mermaid }        { lv_key_indicator } { ls_field-field_name }\n|.
    ENDLOOP.
    lv_mermaid = |{ lv_mermaid }    \}\n\n|.

    " Related entities
    LOOP AT ls_collection-related_entities INTO DATA(ls_related_entity) WHERE view_name IS NOT INITIAL.
      IF lines( ls_related_entity-fields ) > 0.
        lv_mermaid = lv_mermaid && |    { ls_related_entity-view_name } \{\n|.

        " Add all fields for related entity
        LOOP AT ls_related_entity-fields INTO ls_field WHERE field_name IS NOT INITIAL.
          lv_key_indicator = COND string( WHEN ls_field-key_field = abap_true THEN 'KEY' ELSE 'x' ).
          lv_mermaid = |{ lv_mermaid }        { lv_key_indicator } { ls_field-field_name }\n|.
        ENDLOOP.

        lv_mermaid = |{ lv_mermaid }    \}\n\n|.
      ENDIF.
    ENDLOOP.

    " Add relationships using enhanced association information
    LOOP AT ls_collection-all_associations INTO DATA(ls_association) WHERE entity_name IS NOT INITIAL.
      DATA(lv_relationship_type) = COND string(
        WHEN ls_association-relationship = 'COMPOSITION'
        THEN '||--||'
        ELSE '||--|{'
      ).

      DATA(lv_label) = |{ ls_association-relationship } (L{ ls_association-discovery_level })|.

      lv_mermaid = lv_mermaid && |    { ls_association-source_entity } { lv_relationship_type } { ls_association-entity_name } : "{ lv_label }"\n|.
    ENDLOOP.

    " Add summary comment
    lv_mermaid = lv_mermaid && |\n    %% Discovery Summary: { ls_collection-total_entities } entities, depth { ls_collection-discovery_depth }\n|.

    rs_result-success = abap_true.
    rs_result-mermaid_code = lv_mermaid.
  ENDMETHOD.

  METHOD extract_cds_with_related.
    DATA: lt_processed_entities TYPE string_table,
          ls_related_structure  TYPE ty_cds_structure.

    " Extract main entity
    rs_collection-main_entity = extract_cds_metadata( iv_cds_name ).

    " Initialize collection metadata
    rs_collection-discovery_depth = 0.
    rs_collection-total_entities = 1.

    " Only process related entities if main entity is valid
    IF rs_collection-main_entity-has_errors = abap_false.

      " Discover all associations recursively
      rs_collection-all_associations = discover_associations_recur(
        iv_start_entity = iv_cds_name
        iv_max_depth = iv_depth_level
        iv_max_entities = iv_max_entities
      ).

      " Get unique entities from discovered associations
      DATA(lt_unique_entities) = get_unique_entities( rs_collection-all_associations ).

      " Extract metadata for each discovered entity
      LOOP AT lt_unique_entities INTO DATA(lv_entity) WHERE table_line <> iv_cds_name.
        " Limit the number of entities to prevent performance issues
        IF lines( rs_collection-related_entities ) >= iv_max_entities.
          EXIT.
        ENDIF.

        ls_related_structure = extract_cds_metadata( lv_entity ).
        IF ls_related_structure-has_errors = abap_false.
          APPEND ls_related_structure TO rs_collection-related_entities.
        ENDIF.
      ENDLOOP.

      " Update collection metadata
      rs_collection-total_entities = 1 + lines( rs_collection-related_entities ).
      rs_collection-discovery_depth = iv_depth_level.

      " Update main entity with enhanced associations
      rs_collection-main_entity-enhanced_associations = rs_collection-all_associations.
    ENDIF.
  ENDMETHOD.

  METHOD discover_associations_recur.
    DATA: lt_processed_entities TYPE string_table,
          lt_current_level      TYPE string_table,
          lt_next_level         TYPE string_table,
          lv_current_depth      TYPE i VALUE 0.

    CLEAR rt_all_associations.

    " Initialize with start entity
    APPEND iv_start_entity TO lt_processed_entities.
    APPEND iv_start_entity TO lt_current_level.

    " Process each depth level
    WHILE lv_current_depth < iv_max_depth AND lines( lt_current_level ) > 0.
      CLEAR lt_next_level.

      " Process all entities at current level
      LOOP AT lt_current_level INTO DATA(lv_current_entity).
        " Get associations for current entity
        DATA(lt_entity_associations) = get_cds_associations_enhanced(
          iv_cds_name = lv_current_entity
          iv_source_entity = lv_current_entity
          iv_discovery_level = lv_current_depth
        ).

        " Add to result and collect next level entities
        LOOP AT lt_entity_associations INTO DATA(ls_association).
          " Check if we've reached the entity limit
          IF lines( rt_all_associations ) >= iv_max_entities.
            RETURN.
          ENDIF.

          " Add association to result if not already exists
          IF is_association_exists(
               iv_entity_name = ls_association-entity_name
               it_associations = rt_all_associations
             ) = abap_false.
            APPEND ls_association TO rt_all_associations.
          ENDIF.

          " Add to next level if not processed yet
          IF is_entity_processed(
               iv_entity_name = ls_association-entity_name
               it_processed = lt_processed_entities
             ) = abap_false.
            APPEND ls_association-entity_name TO lt_next_level.
            APPEND ls_association-entity_name TO lt_processed_entities.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      " Move to next level
      lt_current_level = lt_next_level.
      lv_current_depth = lv_current_depth + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_cds_associations_enhanced.
    DATA: ls_association_info TYPE ty_association_info.

    CLEAR rt_associations.

    TRY.
        " Convert to proper length for XCO
        DATA(lv_name_c30) = CONV sxco_cds_object_name( iv_cds_name ).
        DATA(lo_view_entity) = xco_cp_cds=>view_entity( lv_name_c30 ).

        " Check if view entity exists
        IF lo_view_entity IS INITIAL.
          RETURN.
        ENDIF.

        " Process associations
        LOOP AT lo_view_entity->associations->all->get( ) INTO DATA(lo_association).
          DATA(ls_association) = lo_association->content( )->get( ).

          " Add target to associations if not empty
          IF ls_association-target IS NOT INITIAL.
            CLEAR ls_association_info.
            ls_association_info-entity_name = ls_association-target.
            ls_association_info-relationship = 'ASSOCIATION'.
            ls_association_info-source_entity = iv_source_entity.
            ls_association_info-discovery_level = iv_discovery_level.
            APPEND ls_association_info TO rt_associations.
          ENDIF.
        ENDLOOP.

        " Process compositions
        LOOP AT lo_view_entity->compositions->all->get( ) INTO DATA(lo_composition).
          DATA(ls_composition) = lo_composition->content( )->get( ).

          " Add alias to compositions if not empty
          IF ls_composition-alias IS NOT INITIAL.
            CLEAR ls_association_info.
            ls_association_info-entity_name = ls_composition-alias.
            ls_association_info-relationship = 'COMPOSITION'.
            ls_association_info-source_entity = iv_source_entity.
            ls_association_info-discovery_level = iv_discovery_level.
            APPEND ls_association_info TO rt_associations.
          ENDIF.
        ENDLOOP.

      CATCH cx_root.
        " On error, return empty associations
        CLEAR rt_associations.
    ENDTRY.
  ENDMETHOD.

  METHOD is_association_exists.
    READ TABLE it_associations TRANSPORTING NO FIELDS
      WITH KEY entity_name = iv_entity_name.
    rv_exists = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD get_unique_entities.
    DATA: lt_temp_entities TYPE string_table.

    " Extract all entity names from associations
    LOOP AT it_associations INTO DATA(ls_association).
      IF ls_association-entity_name IS NOT INITIAL.
        APPEND ls_association-entity_name TO lt_temp_entities.
      ENDIF.
      IF ls_association-source_entity IS NOT INITIAL.
        APPEND ls_association-source_entity TO lt_temp_entities.
      ENDIF.
    ENDLOOP.

    " Remove duplicates
    SORT lt_temp_entities.
    DELETE ADJACENT DUPLICATES FROM lt_temp_entities.

    rt_entities = lt_temp_entities.
  ENDMETHOD.

  METHOD is_entity_processed.
    READ TABLE it_processed TRANSPORTING NO FIELDS
      WITH KEY table_line = iv_entity_name.
    rv_processed = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD extract_cds_metadata.
    " Clear previous errors
    CLEAR: mv_last_error_code, mv_last_error_message, mv_last_error_context.

    " Initialize result structure
    CLEAR rs_structure.
    rs_structure-view_name = iv_cds_name.

    TRY.
        " Validate input Check CDS existence
        IF validate_cds_view( iv_cds_name ) = abap_false.
          rs_structure-has_errors = abap_true.
          rs_structure-error_info-error_code = 'CDS_NOT_FOUND'.
          rs_structure-error_info-error_message = |CDS view '{ iv_cds_name }' not found or not accessible|.
          rs_structure-error_info-error_context = get_system_context( ).
          rs_structure-error_info-timestamp = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
          RETURN.
        ENDIF.

        " Fill description
        rs_structure-description = |CDS View: { iv_cds_name }|.

        " Retrieve fields, associations, and compositions using XCO
        rs_structure-fields = get_cds_fields_xco( iv_cds_name ).
        rs_structure-associations = get_cds_associations_xco( iv_cds_name ).
        rs_structure-compositions = get_cds_compositions_xco( iv_cds_name ).

        " Get enhanced associations for this entity
        rs_structure-enhanced_associations = get_cds_associations_enhanced(
          iv_cds_name = iv_cds_name
          iv_source_entity = iv_cds_name
          iv_discovery_level = 0
        ).

        rs_structure-has_errors = abap_false.

      CATCH cx_root INTO DATA(lx_root).
        rs_structure-has_errors = abap_true.
        rs_structure-error_info-error_code = 'GENERAL_ERROR'.
        rs_structure-error_info-error_message = |Unexpected error: { lx_root->get_text( ) }|.
        rs_structure-error_info-error_context = get_system_context( ).
        rs_structure-error_info-timestamp = cl_abap_tstmp=>utclong2tstmp( utclong_current( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_cds_fields_xco.
    DATA: ls_field TYPE ty_field.

    CLEAR rt_fields.

    TRY.
        " Convert to proper length for XCO
        DATA(lv_name_c30) = CONV sxco_cds_object_name( iv_cds_name ).
        DATA(lo_view_entity) = xco_cp_cds=>view_entity( lv_name_c30 ).

        " Check if view entity exists
        IF lo_view_entity IS INITIAL.
          RETURN.
        ENDIF.

        " Process fields
        LOOP AT lo_view_entity->fields->all->get( ) INTO DATA(lo_field).
          DATA(ls_field_content) = lo_field->content( )->get( ).

          " Only process fields with valid names
          IF ls_field_content-alias IS NOT INITIAL OR ls_field_content-original_name IS NOT INITIAL.
            CLEAR ls_field.

            " Use alias if available, otherwise original name
            ls_field-field_name = COND string(
              WHEN ls_field_content-alias IS NOT INITIAL
              THEN ls_field_content-alias
              ELSE ls_field_content-original_name
            ).

            " Set key field indicator
            ls_field-key_field = ls_field_content-key_indicator.
            ls_field-key_source = COND string(
              WHEN ls_field_content-key_indicator = abap_true
              THEN 'XCO_ANNOTATION'
              ELSE 'NOT_KEY'
            ).

            APPEND ls_field TO rt_fields.
          ENDIF.
        ENDLOOP.

      CATCH cx_root.
        " On error, return empty fields
        CLEAR rt_fields.
    ENDTRY.
  ENDMETHOD.

  METHOD get_cds_associations_xco.
    CLEAR rt_associations.

    TRY.
        " Convert to proper length for XCO
        DATA(lv_name_c30) = CONV sxco_cds_object_name( iv_cds_name ).
        DATA(lo_view_entity) = xco_cp_cds=>view_entity( lv_name_c30 ).

        " Check if view entity exists
        IF lo_view_entity IS INITIAL.
          RETURN.
        ENDIF.

        " Process associations
        LOOP AT lo_view_entity->associations->all->get( ) INTO DATA(lo_association).
          DATA(ls_association) = lo_association->content( )->get( ).

          " Add target to associations if not empty
          IF ls_association-target IS NOT INITIAL.
            READ TABLE rt_associations TRANSPORTING NO FIELDS
              WITH KEY table_line = ls_association-target.
            IF sy-subrc <> 0.
              APPEND ls_association-target TO rt_associations.
            ENDIF.
          ENDIF.
        ENDLOOP.

        " Clean up results
        SORT rt_associations ASCENDING.

      CATCH cx_root.
        " On error, return empty associations
        CLEAR rt_associations.
    ENDTRY.
  ENDMETHOD.

  METHOD get_cds_compositions_xco.
    CLEAR rt_compositions.

    TRY.
        " Convert to proper length for XCO
        DATA(lv_name_c30) = CONV sxco_cds_object_name( iv_cds_name ).
        DATA(lo_view_entity) = xco_cp_cds=>view_entity( lv_name_c30 ).

        " Check if view entity exists
        IF lo_view_entity IS INITIAL.
          RETURN.
        ENDIF.

        " Process compositions
        LOOP AT lo_view_entity->compositions->all->get( ) INTO DATA(lo_composition).
          DATA(ls_composition) = lo_composition->content( )->get( ).

          " Add alias to compositions if not empty
          IF ls_composition-alias IS NOT INITIAL.
            READ TABLE rt_compositions TRANSPORTING NO FIELDS
              WITH KEY table_line = ls_composition-alias.
            IF sy-subrc <> 0.
              APPEND ls_composition-alias TO rt_compositions.
            ENDIF.
          ENDIF.
        ENDLOOP.

        " Clean up results
        SORT rt_compositions ASCENDING.

      CATCH cx_root.
        CLEAR rt_compositions.
    ENDTRY.
  ENDMETHOD.

  METHOD get_object_type.
    rv_type = 'UNKNOWN'.

    TRY.
        " First try as CDS view
        DATA(lv_name_c30) = CONV sxco_cds_object_name( iv_object_name ).
        DATA(lo_view_entity) = xco_cp_cds=>view_entity( lv_name_c30 ).

        IF lo_view_entity->fields->all->get( ) IS NOT INITIAL.
          rv_type = 'CDS'.
          RETURN.
        ENDIF.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD validate_cds_view.
    rv_valid = abap_false.

    TRY.
        IF iv_cds_name IS INITIAL OR strlen( iv_cds_name ) > 30.
          RETURN.
        ENDIF.

        " Use XCO to validate CDS view existence
        DATA(lv_name_c30) = CONV sxco_cds_object_name( iv_cds_name ).
        DATA(lo_view_entity) = xco_cp_cds=>view_entity( lv_name_c30 ).

        " Check if we can access fields
        IF lo_view_entity->fields->all->get( ) IS NOT INITIAL.
          rv_valid = abap_true.
        ENDIF.

      CATCH cx_root.
        rv_valid = abap_false.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
