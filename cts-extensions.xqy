xquery version "1.0-ml";

(:~
 : experimental extensions to MarkLogic cts functionality.
 : Includes new query and lexicon types, and conversion functions for lexicon references.
 :
 : &lt;em&gt;
 :   &lt;strong&gt;Warning: this is experimental software!&lt;/strong&gt;
 :   This module uses un-supported features of MarkLogic Server, which are subject to modification or removal without notice.
 : &lt;/em&gt;
 :
 : @author Joe Bryan
 : @version 1.0.0
 :)
module namespace ctx = "http://marklogic.com/cts-extensions";

declare namespace db = "http://marklogic.com/xdmp/database";
declare namespace qry = "http://marklogic.com/cts/query";
declare namespace err = "http://www.w3.org/2005/xqt-errors";

declare option xdmp:mapping "false";

(:~ numeric lexicon scalar types :)
declare variable $ctx:numeric-scalar-types as xs:string+ :=
("int", "unsignedInt", "long", "unsignedLong", "float", "double", "decimal");

(:
 :
 : %private helper functions for creating and inspecting query plans
 :
 :)

(: creates xquery namespace declarations from sequence of alternating prefixes and URIs :)
declare %private function ctx:with-namespaces($namespaces as xs:string*) as xs:string*
{
  if (fn:count($namespaces) mod 2 eq 1)
  then fn:error((), "XDMP-ARG:", "Invalid argument; odd number of namespaces")
  else
    for $i in 1 to (fn:count($namespaces) div 2)
    let $index := $i * 2 - 1
    return ctx:declare-ns($namespaces[ $index ], $namespaces[ $index + 1 ])
};

(: creates xquery namespace declarations based on 1-or-more QNames :)
declare %private function ctx:declare-ns($qnames as xs:QName*) as xs:string*
{
  for $qname at $i in $qnames
  let $uri := fn:namespace-uri-from-QName($qname)
  where $uri ne ""
  return ctx:declare-ns("_" || fn:string($i), $uri)
};

(: creates xquery namespace declarations :)
declare %private function ctx:declare-ns($prefix as xs:string, $uri as xs:string) as xs:string
{
  'declare namespace ' || $prefix || '= "' || $uri || '";'
};

(: create a namespace-aware string serialization of 1-or-more QNames :)
declare %private function ctx:format-QName($qnames as xs:QName*) as xs:string*
{
  for $qname at $i in $qnames
  let $uri := fn:namespace-uri-from-QName($qname)
  let $prefix := ("_" || fn:string($i) || ":")[$uri ne ""]
  return $prefix || fn:local-name-from-QName($qname)
};

(: return the query plan of the eval'd XPath expression :)
declare %private function ctx:plan($prolog as xs:string?, $query as xs:string) as element(qry:final-plan)
{
  let $query :=
    if (fn:starts-with($query, "/")) then $query
    else "/" || $query
  return xdmp:eval($prolog || "xdmp:plan(" || $query || ")")/qry:final-plan
};

(: return the hash key of the query-plan term where the annotation matches the predicate :)
declare %private function ctx:query-term(
  $plan as element(qry:final-plan),
  $predicate as (function(item()) as xs:boolean)
) as xs:unsignedLong
{
  (: TODO: catch, throw custom error invalid predicate :)
  fn:data(
    fn:exactly-one(
      $plan//qry:term-query[ $predicate(qry:annotation) ]/qry:key ))
};

(:
 :
 : extension query type constructors
 :
 :)

(: calculate the query term hash key of a root QName :)
declare %private function ctx:root-element-query-term($qname as xs:QName) as xs:unsignedLong
{
  ctx:query-term(
    ctx:plan( ctx:declare-ns($qname), ctx:format-QName($qname)),
    fn:starts-with(?, "doc-root("))
};

(:~
 : returns a `cts:query` matching fragments with a given root QName
 :)
declare function ctx:root-element-query($qname as xs:QName) as cts:query
{
  ctx:root-element-query($qname, ())
};

(:~
 : returns a `cts:query` matching fragments with a given root QName, constrained
 : by the query param
 :)
declare function ctx:root-element-query($qname as xs:QName, $query as cts:query?) as cts:query
{
  cts:and-query((
    cts:term-query(
      ctx:root-element-query-term($qname), 0),
    $query))
};

(: calculate the query term hash key of parent/child QNames :)
declare %private function ctx:element-child-query-term($qname as xs:QName, $child as xs:QName) as xs:unsignedLong
{
  let $prolog := fn:string-join(ctx:declare-ns(($qname, $child)), "")
  let $query := fn:string-join(ctx:format-QName(($qname, $child)), "/")
  return
    ctx:query-term(
      ctx:plan($prolog, $query),
      fn:starts-with(?, "element-child("))
};

(:~
 : returns a `cts:query` matching fragments with the given parent/child QNames
 :)
declare function ctx:element-child-query($qname as xs:QName, $child as xs:QName) as cts:query
{
  ctx:element-child-query($qname, $child, ())
};

(:~
 : returns a `cts:query` matching fragments with the given parent/child QNames,
 : constrained by the query param
 :)
declare function ctx:element-child-query(
  $qname as xs:QName,
  $child as xs:QName,
  $query as cts:query?
) as cts:query
{
  cts:and-query((
    cts:term-query(
      ctx:element-child-query-term($qname, $child), 0),
    (: TODO: should this be wrapped in a cts:element-query? :)
    $query))
};

(: calculate the query term hash key of element/attribute QNames :)
declare %private function ctx:element-attribute-query-term($qname as xs:QName, $attr as xs:QName) as xs:unsignedLong
{
  let $prolog := fn:string-join(ctx:declare-ns(($qname, $attr)), "")
  let $query := fn:string-join(ctx:format-QName(($qname, $attr)), "/@")
  return
    ctx:query-term(
      ctx:plan($prolog, $query),
      fn:starts-with(?, "element-attribute("))
};

(:~
 : returns a `cts:query` matching fragments with the given element/attribute QNames
 :)
declare function ctx:element-attribute-query($qname as xs:QName, $attr as xs:QName) as cts:query
{
  ctx:element-attribute-query($qname, $attr, ())
};

(:~
 : returns a `cts:query` matching fragments with the given element/attribute QNames,
 : constrained by the query param
 :)
declare function ctx:element-attribute-query(
  $qname as xs:QName,
  $attr as xs:QName,
  $query as cts:query?
) as cts:query
{
  cts:and-query((
    cts:term-query(
      ctx:element-attribute-query-term($qname, $attr), 0),
    (: TODO: remove? :)
    $query))
};


(:~
 : returns a `cts:query` matching fragments containing `$path-expression`
 :)
declare function ctx:path-query($path-expression as xs:string) as cts:query
{
  ctx:path-query($path-expression, ())
};

(:~
 : returns a `cts:query` matching fragments containing `$path-expression`
 :)
declare function ctx:path-query($path-expression as xs:string, $namespaces as xs:string*) as cts:query
{
  try {
    cts:and-query(
      fn:exactly-one(
        ctx:plan(
          fn:string-join(ctx:with-namespaces($namespaces) , ""),
          $path-expression)
        [../qry:info-trace = "Path is fully searchable."])
        /qry:and-query/qry:term-query/qry:key ! cts:term-query(.) )
  }
  catch err:FORG0005 {
    fn:error((), "XDMP-UNSEARCHABLE", "Expression is unsearchable: " || $path-expression)
  }
};

(:~
 : returns a `cts:query` matching fragments with values of `$type` in `$path-expression`
 : (requires a matching path-range-index)
 :)
declare function ctx:path-query(
  $path-expression as xs:string,
  $scalar-type as xs:string,
  $collation as xs:string?
) as cts:query
{
  let $value :=
    switch($scalar-type)
    case "string" return ""
    case "anyURI" return xs:anyURI("")
    case "dateTime" return fn:current-dateTime()
    case "time" return fn:current-time()
    case "date" return fn:current-date()
    case "gYearMonth" return xs:gYearMonth(fn:current-date())
    case "gYear" return xs:gYear(fn:current-date())
    case "gMonth" return xs:gMonth(fn:current-date())
    case "gDay" return xs:gDay(fn:current-date())
    case "yearMonthDuration" return xs:yearMonthDuration("P1M")
    case "dayTimeDuration" return xs:dayTimeDuration("P1D")
    default return
      if ($scalar-type = $ctx:numeric-scalar-types) then 0
      else fn:error((), "UNKNOWN_TYPE", $scalar-type)
  return cts:or-query((
    cts:path-range-query($path-expression, "=", $value, "collation=" || $collation),
    cts:path-range-query($path-expression, "!=", $value, "collation=" || $collation)
  ))
};

(:
 :
 : pseudo-lexicon functions
 :
 :)

(:~
 : a "pseudo-lexicon" of root-element QNames; returns a sequence of all document-root element QNames
 :)
declare function ctx:root-QNames() as xs:QName*
{
  ctx:root-QNames((), ())
};

(:~
 : a "pseudo-lexicon" of root-element QNames; returns a sequence of document-root element QNames
 : where document matches `$arg`. (if `$arg` is a QName, matches documents containing an element of that name)
 :
 : @param $arg as `cts:query` or `xs:QName`
 :)
declare function ctx:root-QNames($arg) as xs:QName*
{
  let $query :=
    if ($arg instance of cts:query)
    then $arg
    else
      if ($arg instance of xs:QName)
      then cts:element-query($arg, cts:and-query(()))
      else fn:error(xs:QName("UNKNOWN-TYPE"), (xdmp:describe($arg), $arg))
  return ctx:root-QNames($query, ())
};

(:~
 : a "pseudo-lexicon" of root-QNames; returns a sequence of document-root element QNames
 : constrained by `$query`, excluding `$excluded-roots`
 :)
declare function ctx:root-QNames($query as cts:query?, $excluded-roots as xs:QName*) as xs:QName*
{
  let $query :=
    cts:and-query(($query,
      $excluded-roots ! cts:not-query(ctx:root-element-query(.))))
  let $root := cts:search(/*, $query, "unfiltered")[1]/fn:node-name()
  return $root ! (., ctx:root-QNames($query, .))
};

(:
 :
 : public helper functions
 :
 :)

(:~
 : returns the database-path-namespaces as a sequence of alternating namespace prefixes and URIs
 :)
declare function ctx:db-path-namespaces() as xs:string*
{ ctx:db-path-namespaces(xdmp:database()) };

declare function ctx:db-path-namespaces($database-id as xs:unsignedLong) as xs:string*
{
  for $ns in xdmp:database-path-namespaces($database-id)/db:path-namespace
  return (
    $ns/db:prefix/fn:string(),
    $ns/db:namespace-uri/fn:string()
  )
};

(:
 :
 : lexicon reference conversion/constructor functions
 :
 :)

(:~
 : constructs a map from a `cts:reference` object
 :
 : @param $ref as `cts:reference` or `element(cts:*-reference)`
 :)
declare function ctx:reference-to-map($ref) as map:map
{
  let $ref :=
    if ($ref instance of element())
    then $ref
    else document { $ref }/*
  return
    map:new((
      map:entry("ref-type", fn:local-name($ref)),
      for $x in $ref/*
      return map:entry(fn:local-name($x), $x/fn:string())))
};

(:~
 : constructs a `cts:reference` object from a map
 :)
declare function ctx:reference-from-map($map as map:map) as cts:reference?
{
  let $ref-type := map:get($map, "ref-type")
  return
    if (fn:exists($ref-type))
    then
      cts:reference-parse(
        element { "cts:" || $ref-type } {
          for $key in map:keys($map)[. ne "ref-type"]
          return
            element { "cts:" || $key } {
              map:get($map, $key)
            }
        })
    else ()
};

(:~
 : constructs 1-or-more `cts:reference` objects from the given index definition
 :
 : @param $node as `element(db:*-index)` (as returned by the admin API)
 :)
declare function ctx:resolve-reference-from-index($node) as cts:reference*
{
  let $options :=
  (
    $node/db:scalar-type ! fn:concat("type=", .),
    if ($node/db:scalar-type eq "string")
    then $node/db:collation ! fn:concat("collation=", .)
    else ()
    (:
      TODO:
      $node/db:coordinate-system ! fn:concat("coordinate-system", .)
      $node/db:point-format ! fn:concat("type", .)
    :)
  )
  return
    typeswitch($node)
      case element(db:range-element-index) return
        for $elem in fn:tokenize($node/db:localname/fn:string(), " ")
        return cts:element-reference(fn:QName($node/db:namespace-uri, $elem), $options)
      case element(db:range-element-attribute-index) return
        for $elem in fn:tokenize($node/db:parent-localname/fn:string(), " ")
        for $attr in fn:tokenize($node/db:localname/fn:string(), " ")
        return
          cts:element-attribute-reference(
            fn:QName($node/db:parent-namespace-uri, $elem),
            fn:QName($node/db:namespace-uri, $attr),
            $options)
      case element(db:range-path-index) return
        xdmp:with-namespaces(
          ctx:db-path-namespaces(),
          cts:path-reference($node/db:path-expression, $options))
      (: TODO: process other reference types :)
      case element(cts:field-reference) return ()
      case element(cts:uri-reference) return ()
      case element(cts:collection-reference) return ()
      case element(cts:geospatial-attribute-pair-reference) return ()
      case element(cts:geospatial-element-pair-reference) return ()
      case element(cts:geospatial-element-child-reference) return ()
      case element(cts:geospatial-element-reference) return ()
      default return fn:error((),"Unknown Reference Type", $node)
};

(:~
 : constructs a `cts:query` matching fragments that contain a `cts:reference`
 :
 : @param $ref as `cts:reference` or `element(cts:*-reference)`
 :)
declare function ctx:reference-query($ref) as cts:query
{
  let $ref :=
    if ($ref instance of element())
    then $ref
    else document { $ref }/*
  return
    typeswitch($ref)
    case element(cts:element-reference) return
      cts:element-query(fn:QName($ref/cts:namespace-uri, $ref/cts:localname), cts:and-query(()))
    case element(cts:element-attribute-reference) return
      ctx:element-attribute-query(
        fn:QName($ref/cts:parent-namespace-uri, $ref/cts:parent-localname),
        fn:QName($ref/cts:namespace-uri, $ref/cts:localname))
    case element(cts:path-reference) return
      xdmp:with-namespaces(
        ctx:db-path-namespaces(),
        ctx:path-query(
          $ref/cts:path-expression,
          $ref/cts:scalar-type,
          $ref/cts:collation))
    (: TODO:  :)
    case element(cts:field-reference) return ()
    case element(cts:geospatial-attribute-pair-reference) return ()
    case element(cts:geospatial-element-pair-reference) return ()
    case element(cts:geospatial-element-child-reference) return ()
    case element(cts:geospatial-element-reference) return ()

    case element(cts:uri-reference) return
      cts:and-query(())
    case element(cts:collection-reference) return
      fn:error(xs:QName("REFERENCE-QUERY-ERROR"),"cts:collection-reference is semantically meaningless without values", $ref)
    default return fn:error(xs:QName("REFERENCE-QUERY-ERROR"),"Unknown Reference Type to create query", $ref)
};

(:~
 : constructs an `=` range query from a `cts:reference` and 1-or-more values
 :
 : @param $ref as `cts:reference` or `element(cts:*-reference)`
 :)
declare function ctx:reference-query($ref, $values as xs:anyAtomicType*) as cts:query
{
  ctx:reference-query($ref, "=", $values)
};

(:~
 : constructs a range query from a `cts:reference` and 1-or-more values
 :
 : @param $ref as `cts:reference` or `element(cts:*-reference)`
 :)
declare function ctx:reference-query($ref, $operator as xs:string, $values as xs:anyAtomicType*) as cts:query
{
  let $ref :=
    if ($ref instance of element())
    then $ref
    else document { $ref }/*
  (: TODO :)
  let $options := (
    $ref/cts:collation ! fn:concat("collation=", .),
    $ref/cts:coordinate-system ! fn:concat("coordinate-system", .)
  )
  let $constructor :=
    typeswitch($ref)
    case element(cts:element-reference) return
      cts:element-range-query(
        fn:QName($ref/cts:namespace-uri, $ref/cts:localname), ?, ?, ?)
    case element(cts:element-attribute-reference) return
      cts:element-attribute-range-query(
        fn:QName($ref/cts:parent-namespace-uri, $ref/cts:parent-localname),
        fn:QName($ref/cts:namespace-uri, $ref/cts:localname), ?, ?, ?)
    case element(cts:path-reference) return
      xdmp:with-namespaces(
        ctx:db-path-namespaces(),
        cts:path-range-query(
          $ref/cts:path-expression, ?, ?, ?))
    case element(cts:field-reference) return
      cts:field-range-query(
        $ref/cts:name, ?, ?, ?)
    case element(cts:uri-reference) return
      (: TODO: check operator (handle = / !=, error otherwise) :)
      function($operator, $values, $options) {
        cts:document-query($values)
      }
    case element(cts:collection-reference) return
      (: TODO: check operator (handle = / !=, error otherwise) :)
      function($operator, $values, $options) {
        cts:collection-query($values)
      }
    (: TODO:  :)
    case element(cts:geospatial-attribute-pair-reference) return ()
    case element(cts:geospatial-element-pair-reference) return ()
    case element(cts:geospatial-element-child-reference) return ()
    case element(cts:geospatial-element-reference) return ()
    default return fn:error(xs:QName("REFERENCE-QUERY-ERROR"),"Unknown Reference Type to create query", $ref)
  return $constructor($operator, $values, $options)
};
