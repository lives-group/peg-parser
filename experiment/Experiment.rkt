#lang racket

(require "../examples/xml.rkt")

(xml:parse "<?xml x=\"None\"?> 
<note>
  <to>Tove</to>
  <from> Jani </from>
  <n:a> 2021-12-05T15:47:33Z </n:a>
</note> ")