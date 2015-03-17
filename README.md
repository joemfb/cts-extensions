### cts-extensions

##### xquery library module: http://marklogic.com/cts-extensions

 experimental extensions to MarkLogic cts functionality.
 Includes new query and lexicon types, and conversion functions for lexicon references.

 <em>
   <strong>Warning: this is experimental software!</strong>
   This module uses un-supported features of MarkLogic Server, which are subject to modification or removal without notice.
 </em>

Author:  Joe Bryan

Version:  0.1

#### Table of Contents

* Variables: [$ctx:numeric-scalar-types](#var_ctx_numeric-scalar-types)
* Functions: [ctx:root-element-query\#1](#func_ctx_root-element-query_1), [ctx:root-element-query\#2](#func_ctx_root-element-query_2), [ctx:element-child-query\#2](#func_ctx_element-child-query_2), [ctx:element-child-query\#3](#func_ctx_element-child-query_3), [ctx:element-attribute-query\#2](#func_ctx_element-attribute-query_2), [ctx:element-attribute-query\#3](#func_ctx_element-attribute-query_3), [ctx:path-query\#1](#func_ctx_path-query_1), [ctx:path-query\#2](#func_ctx_path-query_2), [ctx:path-query\#3](#func_ctx_path-query_3), [ctx:root-QNames\#0](#func_ctx_root-QNames_0), [ctx:root-QNames\#1](#func_ctx_root-QNames_1), [ctx:root-QNames\#2](#func_ctx_root-QNames_2), [ctx:db-path-namespaces\#0](#func_ctx_db-path-namespaces_0), [ctx:db-path-namespaces\#1](#func_ctx_db-path-namespaces_1), [ctx:resolve-reference-from-index\#1](#func_ctx_resolve-reference-from-index_1), [ctx:reference-query\#1](#func_ctx_reference-query_1), [ctx:reference-query\#2](#func_ctx_reference-query_2), [ctx:reference-query\#3](#func_ctx_reference-query_3)

#### Variables

##### <a name="var_ctx_numeric-scalar-types"/> $ctx:numeric-scalar-types
```xquery
$ctx:numeric-scalar-types as xs:string+
```
 numeric lexicon scalar types 

#### Functions

##### <a name="func_ctx_root-element-query_1"/> ctx:root-element-query\#1
```xquery
ctx:root-element-query($qname as xs:QName) as cts:query
```

 returns a `cts:query` matching fragments with a given root QName

###### params

* $qname as `xs:QName`

###### returns `cts:query`

##### <a name="func_ctx_root-element-query_2"/> ctx:root-element-query\#2
```xquery
ctx:root-element-query($qname as xs:QName, $query as cts:query?) as cts:query
```

 returns a `cts:query` matching fragments with a given root QName, constrained
 by the query param

###### params

* $qname as `xs:QName`
* $query as `cts:query?`

###### returns `cts:query`

##### <a name="func_ctx_element-child-query_2"/> ctx:element-child-query\#2
```xquery
ctx:element-child-query($qname as xs:QName, $child as xs:QName) as cts:query
```

 returns a `cts:query` matching fragments with the given parent/child QNames

###### params

* $qname as `xs:QName`
* $child as `xs:QName`

###### returns `cts:query`

##### <a name="func_ctx_element-child-query_3"/> ctx:element-child-query\#3
```xquery
ctx:element-child-query($qname as xs:QName,  $child as xs:QName,  $query as cts:query?) as cts:query
```

 returns a `cts:query` matching fragments with the given parent/child QNames,
 constrained by the query param

###### params

* $qname as `xs:QName`
* $child as `xs:QName`
* $query as `cts:query?`

###### returns `cts:query`

##### <a name="func_ctx_element-attribute-query_2"/> ctx:element-attribute-query\#2
```xquery
ctx:element-attribute-query($qname as xs:QName, $attr as xs:QName) as cts:query
```

 returns a `cts:query` matching fragments with the given element/attribute QNames

###### params

* $qname as `xs:QName`
* $attr as `xs:QName`

###### returns `cts:query`

##### <a name="func_ctx_element-attribute-query_3"/> ctx:element-attribute-query\#3
```xquery
ctx:element-attribute-query($qname as xs:QName,  $attr as xs:QName,  $query as cts:query?) as cts:query
```

 returns a `cts:query` matching fragments with the given element/attribute QNames,
 constrained by the query param

###### params

* $qname as `xs:QName`
* $attr as `xs:QName`
* $query as `cts:query?`

###### returns `cts:query`

##### <a name="func_ctx_path-query_1"/> ctx:path-query\#1
```xquery
ctx:path-query($path-expression as xs:string) as cts:query
```

 returns a `cts:query` matching fragments containing `$path-expression`

###### params

* $path-expression as `xs:string`

###### returns `cts:query`

##### <a name="func_ctx_path-query_2"/> ctx:path-query\#2
```xquery
ctx:path-query($path-expression as xs:string, $namespaces as xs:string*) as cts:query
```

 returns a `cts:query` matching fragments containing `$path-expression`

###### params

* $path-expression as `xs:string`
* $namespaces as `xs:string*`

###### returns `cts:query`

##### <a name="func_ctx_path-query_3"/> ctx:path-query\#3
```xquery
ctx:path-query($path-expression as xs:string,  $scalar-type as xs:string,  $collation as xs:string) as cts:query
```

 returns a `cts:query` matching fragments with values of `$type` in `$path-expression`
 (requires a matching path-range-index)

###### params

* $path-expression as `xs:string`
* $scalar-type as `xs:string`
* $collation as `xs:string`

###### returns `cts:query`

##### <a name="func_ctx_root-QNames_0"/> ctx:root-QNames\#0
```xquery
ctx:root-QNames() as xs:QName*
```

 a "pseudo-lexicon" of root-element QNames; returns a sequence of all document-root element QNames

###### returns `xs:QName*`

##### <a name="func_ctx_root-QNames_1"/> ctx:root-QNames\#1
```xquery
ctx:root-QNames($arg) as xs:QName*
```

 a "pseudo-lexicon" of root-element QNames; returns a sequence of document-root element QNames
 where document matches `$arg`. (if `$arg` is a QName, matches documents containing an element of that name)

###### params

* $arg as `cts:query` or `xs:QName`

###### returns `xs:QName*`

##### <a name="func_ctx_root-QNames_2"/> ctx:root-QNames\#2
```xquery
ctx:root-QNames($query as cts:query?, $excluded-roots as xs:QName*) as xs:QName*
```

 a "pseudo-lexicon" of root-QNames; returns a sequence of document-root element QNames
 constrained by `$query`, excluding `$excluded-roots`

###### params

* $query as `cts:query?`
* $excluded-roots as `xs:QName*`

###### returns `xs:QName*`

##### <a name="func_ctx_db-path-namespaces_0"/> ctx:db-path-namespaces\#0
```xquery
ctx:db-path-namespaces() as xs:string*
```

 returns the database-path-namespaces as a sequence of alternating namespace prefixes and URIs

###### returns `xs:string*`

##### <a name="func_ctx_db-path-namespaces_1"/> ctx:db-path-namespaces\#1
```xquery
ctx:db-path-namespaces($database-id as xs:unsignedLong) as xs:string*
```

###### params

* $database-id as `xs:unsignedLong`

###### returns `xs:string*`

##### <a name="func_ctx_resolve-reference-from-index_1"/> ctx:resolve-reference-from-index\#1
```xquery
ctx:resolve-reference-from-index($node) as cts:reference*
```

 constructs 1-or-more `cts:reference` objects from the given index definition

###### params

* $node as `element(db:*-index)` (as returned by the admin API)

###### returns `cts:reference*`

##### <a name="func_ctx_reference-query_1"/> ctx:reference-query\#1
```xquery
ctx:reference-query($ref) as cts:query
```

 constructs a `cts:query` matching fragments that contain a `cts:reference`

###### params

* $ref as `cts:reference` or `element(cts:*-reference)`

###### returns `cts:query`

##### <a name="func_ctx_reference-query_2"/> ctx:reference-query\#2
```xquery
ctx:reference-query($ref, $values as xs:anyAtomicType*) as cts:query
```

 constructs an `=` range query from a `cts:reference` and 1-or-more values

###### params

* $ref as `cts:reference` or `element(cts:*-reference)`
* $values as `xs:anyAtomicType*`

###### returns `cts:query`

##### <a name="func_ctx_reference-query_3"/> ctx:reference-query\#3
```xquery
ctx:reference-query($ref, $operator as xs:string, $values as xs:anyAtomicType*) as cts:query
```

 constructs a range query from a `cts:reference` and 1-or-more values

###### params

* $ref as `cts:reference` or `element(cts:*-reference)`
* $operator as `xs:string`
* $values as `xs:anyAtomicType*`

###### returns `cts:query`

*Generated by [xquerydoc](https://github.com/xquery/xquerydoc)*

### License Information

- Copyright (c) 2014 Joseph Bryan. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

[http://www.apache.org/licenses/LICENSE-2.0]
(http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

The use of the Apache License does not indicate that this project is
affiliated with the Apache Software Foundation.
