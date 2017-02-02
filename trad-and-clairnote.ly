%    This file "trad-and-clairnote.ly" is a LilyPond include file for
%    producing sheet music that includes traditional music notation alongside
%    Clairnote music notation (http://clairnote.org).
%    Version: 20150403
%
%    Copyright © 2015 Paul Morris, except for functions copied and modified
%    from LilyPond source code as noted in comments below.
%    Contact information: http://clairnote.org/about/
%
%    This file is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    This file is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with this file.  If not, see <http://www.gnu.org/licenses/>.

\version "2.18.2"

#(use-modules (ice-9 regex))

% Include this file after clairnote-code.ly to allow output that contains both
% Clairnote staves and traditional staves.
% Requires clairnote-code.ly version 20150403 or later.

%% CLEFS

% Copy of c0-pitch-alist with Clairnote clef settings.
% Needed because it is not defined publicly in LilyPond source code.
#(define c0-pitch-alist
   '(("clefs.G" . -7) ;; -7 = -12 minus -5
      ("clefs.C" . 0) ;; unchanged
      ("clefs.F" . 7) ;; 7 = 12 minus 5
      ("clefs.percussion" . 0))) % unchanged

% make-clef-set copied from scm/parser-clef.scm and modified.
% Inserts the clef-name (as it was input by the user), into the
% clef property set, stored in custom context property 'clefInput
% for later retrieval by clefsTrad music function (below).

#(define (make-clef-set clef-name)
   "Generate the clef setting commands for a clef with name @var{clef-name}."
   (let* ((match (string-match "^(.*)([_^])([^0-9a-zA-Z]*)([1-9][0-9]*)([^0-9a-zA-Z]*)$" clef-name))
          (e (assoc-get (if match (match:substring match 1) clef-name) supported-clefs))
          (oct (if match
                   ((if (equal? (match:substring match 2) "^") - +)
                    (1- (string->number (match:substring match 4))))
                   0))
          (style (cond ((not match) 'default)
                   ((equal? (match:substring match 3) "(") 'parenthesized)
                   ((equal? (match:substring match 3) "[") 'bracketed)
                   (else 'default))))
     (if e
         (let ((musics (list
                        ;; added custom context property to store the clef input string
                        (make-property-set 'clefInput clef-name)

                        (make-property-set 'clefGlyph (car e))
                        (make-property-set 'middleCClefPosition
                          (+ oct (cadr e)
                            (assoc-get (car e) c0-pitch-alist)))
                        (make-property-set 'clefPosition (cadr e))
                        (make-property-set 'clefTransposition (- oct))
                        (make-property-set 'clefTranspositionStyle style)
                        (make-apply-context ly:set-middle-C!))))
           (context-spec-music (make-sequential-music musics) 'Staff))
         (begin
          (ly:warning (_ "unknown clef type `~a'") clef-name)
          (ly:warning (_ "supported clefs: ~a")
            (string-join
             (sort (map car supported-clefs) string<?)))
          (make-music 'Music)))))


% Exact copy of make-cue-clef-set from scm/parser-clef.scm
% needed here so that it calls the customized make-clef-set above.
#(define (make-cue-clef-set clef-name)
   "Generate the clef setting commands for a cue clef with name
@var{clef-name}."
   (define cue-clef-map
     '((clefGlyph . cueClefGlyph)
       (middleCClefPosition . middleCCuePosition)
       (clefPosition . cueClefPosition)
       (clefTransposition . cueClefTransposition)
       (clefTranspositionStyle . cueClefTranspositionStyle)))
   (let ((clef (make-clef-set clef-name)))
     (for-each
      (lambda (m)
        (let ((mapped (assq-ref cue-clef-map
                        (ly:music-property m 'symbol))))
          (if mapped
              (set! (ly:music-property m 'symbol) mapped))))
      (extract-named-music clef 'PropertySet))
     clef))


% trad clef settings copied from scm/parser-clef.scm

#(define trad-supported-clefs
   '(("treble" . ("clefs.G" -2 0))
     ("violin" . ("clefs.G" -2 0))
     ("G" . ("clefs.G" -2 0))
     ("G2" . ("clefs.G" -2 0))
     ("french" . ("clefs.G" -4 0))
     ("soprano" . ("clefs.C" -4 0))
     ("mezzosoprano" . ("clefs.C" -2 0))
     ("alto" . ("clefs.C" 0 0))
     ("C" . ("clefs.C" 0 0))
     ("tenor" . ("clefs.C" 2 0))
     ("baritone" . ("clefs.C" 4 0))
     ("varbaritone" . ("clefs.F" 0 0))
     ("bass" . ("clefs.F" 2 0))
     ("F" . ("clefs.F" 2 0))
     ("subbass" . ("clefs.F" 4 0))
     ("percussion" . ("clefs.percussion" 0 0))
     ("tab" . ("clefs.tab" 0 0))

     ;; should move mensural stuff to separate file?
     ("vaticana-do1" . ("clefs.vaticana.do" -1 0))
     ("vaticana-do2" . ("clefs.vaticana.do" 1 0))
     ("vaticana-do3" . ("clefs.vaticana.do" 3 0))
     ("vaticana-fa1" . ("clefs.vaticana.fa" -1 0))

     ("vaticana-fa2" . ("clefs.vaticana.fa" 1 0))
     ("medicaea-do1" . ("clefs.medicaea.do" -1 0))
     ("medicaea-do2" . ("clefs.medicaea.do" 1 0))
     ("medicaea-do3" . ("clefs.medicaea.do" 3 0))
     ("medicaea-fa1" . ("clefs.medicaea.fa" -1 0))
     ("medicaea-fa2" . ("clefs.medicaea.fa" 1 0))
     ("hufnagel-do1" . ("clefs.hufnagel.do" -1 0))
     ("hufnagel-do2" . ("clefs.hufnagel.do" 1 0))
     ("hufnagel-do3" . ("clefs.hufnagel.do" 3 0))
     ("hufnagel-fa1" . ("clefs.hufnagel.fa" -1 0))
     ("hufnagel-fa2" . ("clefs.hufnagel.fa" 1 0))
     ("hufnagel-do-fa" . ("clefs.hufnagel.do.fa" 4 0))
     ("mensural-c1" . ("clefs.mensural.c" -4 0))
     ("mensural-c2" . ("clefs.mensural.c" -2 0))
     ("mensural-c3" . ("clefs.mensural.c" 0 0))
     ("mensural-c4" . ("clefs.mensural.c" 2 0))
     ("mensural-c5" . ("clefs.mensural.c" 4 0))
     ("blackmensural-c1" . ("clefs.blackmensural.c" -4 0))
     ("blackmensural-c2" . ("clefs.blackmensural.c" -2 0))
     ("blackmensural-c3" . ("clefs.blackmensural.c" 0 0))
     ("blackmensural-c4" . ("clefs.blackmensural.c" 2 0))
     ("blackmensural-c5" . ("clefs.blackmensural.c" 4 0))
     ("mensural-f" . ("clefs.mensural.f" 2 0))
     ("mensural-g" . ("clefs.mensural.g" -2 0))
     ("neomensural-c1" . ("clefs.neomensural.c" -4 0))
     ("neomensural-c2" . ("clefs.neomensural.c" -2 0))
     ("neomensural-c3" . ("clefs.neomensural.c" 0 0))
     ("neomensural-c4" . ("clefs.neomensural.c" 2 0))
     ("neomensural-c5" . ("clefs.neomensural.c" 4 0))
     ("petrucci-c1" . ("clefs.petrucci.c1" -4 0))
     ("petrucci-c2" . ("clefs.petrucci.c2" -2 0))
     ("petrucci-c3" . ("clefs.petrucci.c3" 0 0))
     ("petrucci-c4" . ("clefs.petrucci.c4" 2 0))
     ("petrucci-c5" . ("clefs.petrucci.c5" 4 0))
     ("petrucci-f3" . ("clefs.petrucci.f" 0 0))
     ("petrucci-f4" . ("clefs.petrucci.f" 2 0))
     ("petrucci-f5" . ("clefs.petrucci.f" 4 0))
     ("petrucci-f" . ("clefs.petrucci.f" 2 0))
     ("petrucci-g" . ("clefs.petrucci.g" -2 0))
     ("kievan-do" . ("clefs.kievan.do" 0 0))))

% "an alist mapping GLYPHNAME to the position of the middle C for
% that symbol"
#(define trad-c0-pitch-alist
   '(("clefs.G" . -4)
     ("clefs.GG" . 3)
     ("clefs.tenorG" . 3)
     ("clefs.C" . 0)
     ("clefs.varC" . 0)
     ("clefs.F" . 4)
     ("clefs.percussion" . 0)
     ("clefs.varpercussion" . 0)
     ("clefs.tab" . 0 )
     ("clefs.vaticana.do" . 0)
     ("clefs.vaticana.fa" . 4)
     ("clefs.medicaea.do" . 0)
     ("clefs.medicaea.fa" . 4)
     ("clefs.hufnagel.do" . 0)
     ("clefs.hufnagel.fa" . 4)
     ("clefs.hufnagel.do.fa" . 0)
     ("clefs.mensural.c" . 0)
     ("clefs.mensural.f" . 4)
     ("clefs.mensural.g" . -4)
     ("clefs.blackmensural.c" . 0)
     ("clefs.neomensural.c" . 0)
     ("clefs.petrucci.c1" . 0)
     ("clefs.petrucci.c2" . 0)
     ("clefs.petrucci.c3" . 0)
     ("clefs.petrucci.c4" . 0)
     ("clefs.petrucci.c5" . 0)
     ("clefs.petrucci.f" . 4)
     ("clefs.petrucci.g" . -4)
     ("clefs.kievan.do" . 0)))


#(define (cn-get-clef-data clef-name)
   "Generate the trad clef settings for a clef with name @var{clef-name}.
    A modified copy of make-clef-set from scm/parser-clef.scm
    Takes clef-name input string (with Clairnote transposition number)
    and returns trad clef data.  Uses trad clef settings from this file."

   (let* ((match (string-match "^(.*)([_^])([^0-9a-zA-Z]*)([1-9][0-9]*)([^0-9a-zA-Z]*)$" clef-name))
          (e (assoc-get (if match (match:substring match 1) clef-name) trad-supported-clefs))

          ;; converts transposition number from clairnote value to trad with (round (* 7/12 ...
          ;; ex: 13 from clef-name "treble^13" becomes 8
          ;; ex: 25 from clef-name "bass_25" becomes 15
          (oct (if match
                   ((if (equal? (match:substring match 2) "^") - +)
                    (1- (round (* 7/12 (string->number (match:substring match 4))))))
                   0))

          (style (cond ((not match) 'default)
                   ((equal? (match:substring match 3) "(") 'parenthesized)
                   ((equal? (match:substring match 3) "[") 'bracketed)
                   (else 'default)))

          (clef-glyph (car e))
          (middle-c-clef-position (+ oct (cadr e)
                                    (assoc-get (car e) trad-c0-pitch-alist)))
          (clef-position (cadr e))
          (clef-transposition (- oct)))

     (list clef-glyph middle-c-clef-position clef-position clef-transposition)))


clefsTrad =
#(define-music-function (parser location mus) (ly:music?)
   "Takes music with Clairnote clef settings, and returns music
    with clef settings adjusted for use on a trad staff."
   (let ((clef-data '()))
     (music-map
      (lambda (m)
        (let ((sym (ly:music-property m 'symbol)))
          (cond

           ((eq? 'clefInput sym)
            (set! clef-data (cn-get-clef-data (ly:music-property m 'value))))

           ((or (eq? 'clefGlyph sym) (eq? 'cueClefGlyph sym))
            (ly:music-set-property! m 'value (list-ref clef-data 0)))

           ((or (eq? 'middleCClefPosition sym) (eq? 'middleCCuePosition sym))
            (ly:music-set-property! m 'value (list-ref clef-data 1)))

           ((or (eq? 'clefPosition sym) (eq? 'cueClefPosition sym))
            (ly:music-set-property! m 'value (list-ref clef-data 2)))

           ((or (eq? 'clefTransposition sym) (eq? 'cueClefTransposition sym))
            (ly:music-set-property! m 'value (list-ref clef-data 3)))))

        m)
      mus)))


%% CUSTOM CONTEXT PROPERTIES

% function copied from scm/define-context-properties.scm and modified
#(define (cn-translator-property-description symbol type?)
   (set-object-property! symbol 'translation-type? type?)
   (set-object-property! symbol 'translation-doc "custom context property")
   (set! all-translation-properties (cons symbol all-translation-properties))
   symbol)

% custom context property to store the name of a clef (or cue clef) as input by user
#(cn-translator-property-description 'clefInput string?)


%% STAFF CONTEXT DEFINITION

% recreate Staff context with its standard settings as
% a custom context called \StaffTrad

\layout {
  \context {
    % copied from ly/engraver-init.ly
    \type "Engraver_group"
    \name "Staff"

    \consists "Output_property_engraver"
    \consists "Bar_engraver"
    \consists "Pure_from_neighbor_engraver"
    %% Bar_engraver must be first so default bars aren't overwritten
    %% with empty ones.

    \consists "Font_size_engraver"
    \consists "Separating_line_group_engraver"
    \consists "Dot_column_engraver"
    \consists "Staff_collecting_engraver"

    \consists "Ottava_spanner_engraver"
    \consists "Clef_engraver"
    \consists "Key_engraver"
    \consists "Time_signature_engraver"
    \consists "Ledger_line_engraver"
    \consists "Staff_symbol_engraver"
    \consists "Collision_engraver"
    \consists "Grob_pq_engraver"
    \consists "Rest_collision_engraver"
    \consists "Accidental_engraver"
    \consists "Piano_pedal_engraver"
    \consists "Piano_pedal_align_engraver"
    \consists "Instrument_name_engraver"
    \consists "Axis_group_engraver"
    \consists "Figured_bass_engraver"
    \consists "Figured_bass_position_engraver"
    \consists "Script_row_engraver"
    \consists "Cue_clef_engraver"
    \consists "Fingering_column_engraver"

    localKeySignature = #'()
    createSpacing = ##t
    ignoreFiguredBassRest = ##f

    %% explicitly set instrument, so we don't get weird effects
    %% when doing instrument names for piano staves
    instrumentName = #'()
    shortInstrumentName = #'()

    \defaultchild "Voice"
    \accepts "Voice"
    \accepts "CueVoice"
    \accepts "NullVoice"

    \description "Handles clefs, bar lines, keys, accidentals.  It can contain
@code{Voice} contexts."

    % begin customizations
    \name StaffTrad
    \alias Staff
    % custom grob property
    \override StaffSymbol.cn-is-clairnote-staff = ##f
  }
  % allow parent contexts to accept \StaffTrad
  \context { \Score \accepts StaffTrad }
  \context { \ChoirStaff \accepts StaffTrad }
  \context { \GrandStaff \accepts StaffTrad }
  \context { \PianoStaff \accepts StaffTrad }
  \context { \StaffGroup \accepts StaffTrad }
}

% allow parent contexts to accept \StaffTrad in midi output too
\midi {
  \context {
    \Staff
    \name StaffTrad
    \alias Staff
  }
  \context { \Score \accepts StaffTrad }
  \context { \ChoirStaff \accepts StaffTrad }
  \context { \GrandStaff \accepts StaffTrad }
  \context { \PianoStaff \accepts StaffTrad }
  \context { \StaffGroup \accepts StaffTrad }
}
