syntax match notesCancelledItem /^\(\s\+\).*\<CANCELLED\>.*\(\n\1\s.*\)*/ contains=@notesInline
syntax match notesCancelledMarker /\<CANCELLED\>/ containedin=notesCancelledItem
highlight def link notesCancelledItem notesDoneItem
highlight def link notesCancelledMarker Question
