|%
+$  hoon                                                ::  hoon AST
  $~  [%zpzp ~]                                         ::
  $^  [p=hoon q=hoon]                                   ::
  $%                                                    ::
    [%$ p=axis]                                         ::  simple leg
  ::                                                    ::
    [%base p=base]                                      ::  base spec
    [%bust p=base]                                      ::  bunt base
    [%dbug p=spot q=hoon]                               ::  debug info in trace
    [%eror p=tape]                                      ::  assembly error
    [%hand p=type q=nock]                               ::  premade result
    [%note p=note q=hoon]                               ::  annotate
    [%fits p=hoon q=wing]                               ::  underlying ?=
    [%knit p=(list woof)]                               ::  assemble string
    [%leaf p=(pair term @)]                             ::  symbol spec
    [%limb p=term]                                      ::  take limb
    [%lost p=hoon]                                      ::  not to be taken
    [%rock p=term q=*]                                  ::  fixed constant
    [%sand p=term q=*]                                  ::  unfixed constant
    [%tell p=(list hoon)]                               ::  render as tape
    [%tune p=$@(term tune)]                             ::  minimal face
    [%wing p=wing]                                      ::  take wing
    [%yell p=(list hoon)]                               ::  render as tank
    [%xray p=manx:hoot]                                 ::  ;foo; templating
  ::                                            ::::::  cores
    [%brbc sample=(lest term) body=spec]                ::  |$
    [%brcb p=spec q=alas r=(map term tome)]             ::  |_
    [%brcl p=hoon q=hoon]                               ::  |:
    [%brcn p=(unit term) q=(map term tome)]             ::  |%
    [%brdt p=hoon]                                      ::  |.
    [%brkt p=hoon q=(map term tome)]                    ::  |^
    [%brhp p=hoon]                                      ::  |-
    [%brsg p=spec q=hoon]                               ::  |~
    [%brtr p=spec q=hoon]                               ::  |*
    [%brts p=spec q=hoon]                               ::  |=
    [%brpt p=(unit term) q=(map term tome)]             ::  |@
    [%brwt p=hoon]                                      ::  |?
  ::                                            ::::::  tuples
    [%clcb p=hoon q=hoon]                               ::  :_ [q p]
    [%clkt p=hoon q=hoon r=hoon s=hoon]                 ::  :^ [p q r s]
    [%clhp p=hoon q=hoon]                               ::  :- [p q]
    [%clls p=hoon q=hoon r=hoon]                        ::  :+ [p q r]
    [%clsg p=(list hoon)]                               ::  :~ [p ~]
    [%cltr p=(list hoon)]                               ::  :* p as a tuple
  ::                                            ::::::  invocations
    [%cncb p=wing q=(list (pair wing hoon))]            ::  %_
    [%cndt p=hoon q=hoon]                               ::  %.
    [%cnhp p=hoon q=hoon]                               ::  %-
    [%cncl p=hoon q=(list hoon)]                        ::  %:
    [%cntr p=wing q=hoon r=(list (pair wing hoon))]     ::  %*
    [%cnkt p=hoon q=hoon r=hoon s=hoon]                 ::  %^
    [%cnls p=hoon q=hoon r=hoon]                        ::  %+
    [%cnsg p=wing q=hoon r=(list hoon)]                 ::  %~
    [%cnts p=wing q=(list (pair wing hoon))]            ::  %=
  ::                                            ::::::  nock
    [%dtkt p=spec q=hoon]                               ::  .^  nock 11
    [%dtls p=hoon]                                      ::  .+  nock 4
    [%dttr p=hoon q=hoon]                               ::  .*  nock 2
    [%dtts p=hoon q=hoon]                               ::  .=  nock 5
    [%dtwt p=hoon]                                      ::  .?  nock 3
  ::                                            ::::::  type conversion
    [%ktbr p=hoon]                                      ::  ^|  contravariant
    [%ktdt p=hoon q=hoon]                               ::  ^.  self-cast
    [%ktls p=hoon q=hoon]                               ::  ^+  expression cast
    [%kthp p=spec q=hoon]                               ::  ^-  structure cast
    [%ktpm p=hoon]                                      ::  ^&  covariant
    [%ktsg p=hoon]                                      ::  ^~  constant
    [%ktts p=skin q=hoon]                               ::  ^=  label
    [%ktwt p=hoon]                                      ::  ^?  bivariant
    [%kttr p=spec]                                      ::  ^*  example
    [%ktcl p=spec]                                      ::  ^:  filter
  ::                                            ::::::  hints
    [%sgbr p=hoon q=hoon]                               ::  ~|  sell on trace
    [%sgcb p=hoon q=hoon]                               ::  ~_  tank on trace
    [%sgcn p=chum q=hoon r=tyre s=hoon]                 ::  ~%  general jet hint
    [%sgfs p=chum q=hoon]                               ::  ~/  function j-hint
    [%sggl p=$@(term [p=term q=hoon]) q=hoon]           ::  ~<  backward hint
    [%sggr p=$@(term [p=term q=hoon]) q=hoon]           ::  ~>  forward hint
    [%sgbc p=term q=hoon]                               ::  ~$  profiler hit
    [%sgls p=@ q=hoon]                                  ::  ~+  cache=memoize
    [%sgpm p=@ud q=hoon r=hoon]                         ::  ~&  printf=priority
    [%sgts p=hoon q=hoon]                               ::  ~=  don't duplicate
    [%sgwt p=@ud q=hoon r=hoon s=hoon]                  ::  ~?  tested printf
    [%sgzp p=hoon q=hoon]                               ::  ~!  type on trace
  ::                                            ::::::  miscellaneous
    [%mcts p=marl:hoot]                                 ::  ;=  list templating
    [%mccl p=hoon q=(list hoon)]                        ::  ;:  binary to nary
    [%mcfs p=hoon]                                      ::  ;/  [%$ [%$ p ~] ~]
    [%mcgl p=spec q=hoon r=hoon s=hoon]                 ::  ;<  bind
    [%mcgr p=spec q=hoon r=hoon]                        ::  ;>
    [%mcsg p=hoon q=(list hoon)]                        ::  ;~  kleisli arrow
    [%mcmc p=spec q=hoon]                               ::  ;;  normalize
  ::                                            ::::::  compositions
    [%tsbr p=spec q=hoon]                               ::  =|  push bunt
    [%tscl p=(list (pair wing hoon)) q=hoon]            ::  =:  q w= p changes
    [%tsfs p=skin q=hoon r=hoon]                        ::  =/  typed variable
    [%tsmc p=skin q=hoon r=hoon]                        ::  =;  =/(q p r)
    [%tsdt p=wing q=hoon r=hoon]                        ::  =.  r with p as q
    [%tswt p=wing q=hoon r=hoon s=hoon]                 ::  =?  conditional =.
    [%tsgl p=hoon q=hoon]                               ::  =<  =>(q p)
    [%tshp p=hoon q=hoon]                               ::  =-  =+(q p)
    [%tsgr p=hoon q=hoon]                               ::  =>  q w=subject p
    [%tskt p=skin q=wing r=hoon s=hoon]                 ::  =^  state machine
    [%tsls p=hoon q=hoon]                               ::  =+  q w=[p subject]
    [%tssg p=(list hoon)]                               ::  =~  hoon stack
    [%tstr p=(pair term (unit spec)) q=hoon r=hoon]     ::  =*  new style
    [%tscm p=hoon q=hoon]                               ::  =,  overload p in q
  ::                                            ::::::  conditionals
    [%wtbr p=(list hoon)]                               ::  ?|  loobean or
    [%wthp p=wing q=(list (pair spec hoon))]            ::  ?-  pick case in q
    [%wtcl p=hoon q=hoon r=hoon]                        ::  ?:  if=then=else
    [%wtdt p=hoon q=hoon r=hoon]                        ::  ?.  ?:(p r q)
    [%wtkt p=wing q=hoon r=hoon]                        ::  ?^  if p is a cell
    [%wtgl p=hoon q=hoon]                               ::  ?<  ?:(p !! q)
    [%wtgr p=hoon q=hoon]                               ::  ?>  ?:(p q !!)
    [%wtls p=wing q=hoon r=(list (pair spec hoon))]     ::  ?+  ?-  w=default
    [%wtpm p=(list hoon)]                               ::  ?&  loobean and
    [%wtpt p=wing q=hoon r=hoon]                        ::  ?@  if p is atom
    [%wtsg p=wing q=hoon r=hoon]                        ::  ?~  if p is null
    [%wthx p=skin q=wing]                               ::  ?#  if q matches p
    [%wtts p=spec q=wing]                               ::  ?=  if q matches p
    [%wtzp p=hoon]                                      ::  ?!  loobean not
  ::                                            ::::::  special
    [%zpcm p=hoon q=hoon]                               ::  !,
    [%zpgr p=hoon]                                      ::  !>
    [%zpgl p=spec q=hoon]                               ::  !<
    [%zpmc p=hoon q=hoon]                               ::  !;
    [%zpts p=hoon]                                      ::  !=
    [%zppt p=(list wing) q=hoon r=hoon]                 ::  !@
    [%zpwt p=$@(p=@ [p=@ q=@]) q=hoon]                  ::  !?
    [%zpzp ~]                                           ::  !!
  ==                                                    ::
::
++  ap                                                  ::  hoon engine
  ~%    %ap
      +>+
    ==
      %open  open
      %rake  rake
    ==
  |_  gen=hoon
  ::
  ++  grip
    |=  =skin
    =|  rel=wing
    |-  ^-  hoon
    ?-    skin
        @
      [%tsgl [%tune skin] gen]
        [%base *]
      ?:  ?=(%noun base.skin)
        gen
      [%kthp skin gen]
    ::
        [%cell *]
      =+  haf=~(half ap gen)
      ?^  haf
        :-  $(skin skin.skin, gen p.u.haf)
        $(skin ^skin.skin, gen q.u.haf)
      :+  %tsls
        gen
      :-  $(skin skin.skin, gen [%$ 4])
      $(skin ^skin.skin, gen [%$ 5])
    ::
        [%dbug *]
      [%dbug spot.skin $(skin skin.skin)]
    ::
        [%leaf *]
      [%kthp skin gen]
    ::
        [%help *]
      [%note [%help help.skin] $(skin skin.skin)]
    ::
        [%name *]
      [%tsgl [%tune term.skin] $(skin skin.skin)]
    ::
        [%over *]
      $(skin skin.skin, rel (weld wing.skin rel))
    ::
        [%spec *]
      :+  %kthp
        ?~(rel spec.skin [%over rel spec.skin])
      $(skin skin.skin)
    ::
        [%wash *]
      :+  %tsgl
        :-  %wing
        |-  ^-  wing
        ?:  =(0 depth.skin)  ~
        [[%| 0 ~] $(depth.skin (dec depth.skin))]
      gen
    ==
  ::
  ++  name
    |-  ^-  (unit term)
    ?+  gen  ~
      [%wing *]  ?~  p.gen  ~
                 ?^  i.p.gen
                   ?:(?=(%& -.i.p.gen) ~ q.i.p.gen)
                 `i.p.gen
      [%limb *]  `p.gen
      [%dbug *]  $(gen ~(open ap gen))
      [%tsgl *]  $(gen ~(open ap gen))
      [%tsgr *]  $(gen q.gen)
    ==
  ::
  ++  feck
    |-  ^-  (unit term)
    ?-  gen
      [%sand %tas @]  [~ q.gen]
      [%dbug *]       $(gen q.gen)
      *               ~
    ==
  ::
  ::  not used at present; see comment at %csng in ++open
::::
::++  hail
::  |=  axe=axis
::  =|  air=(list (pair wing hoon))
::  |-  ^+  air
::  =+  hav=half
::  ?~  hav  [[[[%| 0 ~] [%& axe] ~] gen] air]
::  $(gen p.u.hav, axe (peg axe 2), air $(gen q.u.hav, axe (peg axe 3)))
::
  ++  half
    |-  ^-  (unit (pair hoon hoon))
    ?+  gen  ~
      [^ *]       `[p.gen q.gen]
      [%dbug *]   $(gen q.gen)
      [%clcb *]   `[q.gen p.gen]
      [%clhp *]   `[p.gen q.gen]
      [%clkt *]   `[p.gen %clls q.gen r.gen s.gen]
      [%clsg *]   ?~(p.gen ~ `[i.p.gen %clsg t.p.gen])
      [%cltr *]   ?~  p.gen  ~
                  ?~(t.p.gen $(gen i.p.gen) `[i.p.gen %cltr t.p.gen])
    ==
::::
  ::  +flay: hoon to skin
  ::
  ++  flay
    |-  ^-  (unit skin)
    ?+    gen
      =+(open ?:(=(- gen) ~ $(gen -)))
    ::
        [^ *]
      =+  [$(gen p.gen) $(gen q.gen)]
      ?~(-< ~ ?~(-> ~ `[%cell -<+ ->+]))
    ::
        [%base *]
      `gen
    ::
        [%rock *]
      ?@(q.gen `[%leaf p.gen q.gen] ~)
    ::
        [%cnts [@ ~] ~]
      `i.p.gen
    ::
        [%tsgr *]
      %+  biff  reek(gen p.gen)
      |=  =wing
      (bind ^$(gen q.gen) |=(=skin [%over wing skin]))
    ::
        [%limb @]
      `p.gen
    ::
      ::  [%rock *]
      ::  [%spec %leaf q.gen q.gen]
    ::
        [%note [%help *] *]
      (bind $(gen q.gen) |=(=skin [%help p.p.gen skin]))
    ::
        [%wing *]
      ?:  ?=([@ ~] p.gen)
        `i.p.gen
      =/  depth  0
      |-  ^-  (unit skin)
      ?~  p.gen  `[%wash depth]
      ?.  =([%| 0 ~] i.p.gen)  ~
      $(p.gen t.p.gen)
    ::
        [%kttr *]
      `[%spec p.gen %base %noun]
    ::
        [%ktts *]
      %+  biff  $(gen q.gen)
      |=  =skin
      ?@  p.gen  `[%name p.gen skin]
      ?.  ?=([%name @ [%base %noun]] p.gen)  ~
      `[%name term.p.gen skin]
    ==
  ::
  ::  +open: desugarer
  ++  open
    ^-  hoon
    ?-    gen
        [~ *]     [%cnts [[%& p.gen] ~] ~]
    ::
        [%base *]  ~(factory ax `spec`gen)
        [%bust *]  ~(example ax %base p.gen)
        [%ktcl *]  ~(factory ax p.gen)
        [%dbug *]   q.gen
        [%eror *]  ~_((crip p.gen) !!)
    ::
        [%knit *]                                       ::
      :+  %tsgr  [%ktts %v %$ 1]                        ::  =>  v=.
      :-  %brhp                                         ::  |-
      :+  %ktls                                         ::  ^+
        :-  %brhp                                       ::  |-
        :^    %wtcl                                     ::  ?:
            [%bust %flag]                               ::  ?
          [%bust %null]                                 ::  ~
        :-  [%ktts %i [%sand 'tD' *@]]                  ::  :-  i=~~
        [%ktts %t [%limb %$]]                           ::  t=$
      |-  ^-  hoon                                      ::
      ?~  p.gen                                         ::
        [%bust %null]                                   ::  ~
      =+  res=$(p.gen t.p.gen)                          ::
      ^-  hoon                                          ::
      ?@  i.p.gen                                       ::
        [[%sand 'tD' i.p.gen] res]                      ::  [~~{i.p.gen} {res}]
      :+  %tsls                                         ::
        :-  :+  %ktts                                   ::  ^=
              %a                                        ::  a
            :+  %ktls                                   ::  ^+
              [%limb %$]                                ::  $
            [%tsgr [%limb %v] p.i.p.gen]                ::  =>(v {p.i.p.gen})
        [%ktts %b res]                                  ::  b=[res]
      ^-  hoon                                          ::
      :-  %brhp                                         ::  |-
      :^    %wtpt                                       ::  ?@
          [%a ~]                                        ::  a
        [%limb %b]                                      ::  b
      :-  [%tsgl [%$ 2] [%limb %a]]                     ::  :-  -.a
      :+  %cnts                                         ::  %=
        [%$ ~]                                          ::  $
      [[[%a ~] [%tsgl [%$ 3] [%limb %a]]] ~]            ::  a  +.a
    ::
        [%leaf *]  ~(factory ax `spec`gen)
        [%limb *]  [%cnts [p.gen ~] ~]
        [%tell *]  [%cncl [%limb %noah] [%zpgr [%cltr p.gen]] ~]
        [%wing *]  [%cnts p.gen ~]
        [%yell *]  [%cncl [%limb %cain] [%zpgr [%cltr p.gen]] ~]
        [%note *]  q.gen
    ::
    ::TODO: does %gist need to be special cased here?
        [%brbc *]  =-  ?~  -  !!
                       :+  %brtr
                         [%bccl -]
                       |-
                       ?.  ?=([%gist *] body.gen)
                         [%ktcl body.gen]
                       [%note p.body.gen $(body.gen q.body.gen)]
                   %+  turn  `(list term)`sample.gen
                   |=  =term
                   ^-  spec
                   =/  tar  [%base %noun]
                   [%bcts term [%bcsg tar [%bchp tar tar]]]
        [%brcb *]  :+  %tsls  [%kttr p.gen]
                   :+  %brcn  ~
                   %-  ~(run by r.gen)
                   |=  =tome
                   :-  p.tome
                   %-  ~(run by q.tome)
                   |=  =hoon
                   ?~  q.gen  hoon
                   [%tstr [p.i.q.gen ~] q.i.q.gen $(q.gen t.q.gen)]
        [%brcl *]  [%tsls p.gen [%brdt q.gen]]
        [%brdt *]  :+  %brcn  ~
                   =-  [[%$ ~ -] ~ ~]
                   (~(put by *(map term hoon)) %$ p.gen)
        [%brkt *]  :+  %tsgl  [%limb %$]
                   :+  %brcn  ~
                   =+  zil=(~(get by q.gen) %$)
                   ?~  zil
                     %+  ~(put by q.gen)  %$
                     [*what [[%$ p.gen] ~ ~]]
                   %+  ~(put by q.gen)  %$
                   [p.u.zil (~(put by q.u.zil) %$ p.gen)]
        [%brhp *]  [%tsgl [%limb %$] [%brdt p.gen]]
        [%brsg *]  [%ktbr [%brts p.gen q.gen]]
        [%brtr *]  :+  %tsls  [%kttr p.gen]
                   :+  %brpt  ~
                   =-  [[%$ ~ -] ~ ~]
                   (~(put by *(map term hoon)) %$ q.gen)
        [%brts *]  :+  %brcb  p.gen
                   =-  [~ [[%$ ~ -] ~ ~]]
                   (~(put by *(map term hoon)) %$ q.gen)
        [%brwt *]  [%ktwt %brdt p.gen]
    ::
        [%clkt *]  [p.gen q.gen r.gen s.gen]
        [%clls *]  [p.gen q.gen r.gen]
        [%clcb *]  [q.gen p.gen]
        [%clhp *]  [p.gen q.gen]
        [%clsg *]
      |-  ^-  hoon
      ?~  p.gen
        [%rock %n ~]
      [i.p.gen $(p.gen t.p.gen)]
    ::
        [%cltr *]
      |-  ^-  hoon
      ?~  p.gen
        [%zpzp ~]
      ?~  t.p.gen
        i.p.gen
      [i.p.gen $(p.gen t.p.gen)]
    ::
        [%kttr *]  [%ktsg ~(example ax p.gen)]
        [%cncb *]  [%ktls [%wing p.gen] %cnts p.gen q.gen]
        [%cndt *]  [%cncl q.gen [p.gen ~]]
        [%cnkt *]  [%cncl p.gen q.gen r.gen s.gen ~]
        [%cnls *]  [%cncl p.gen q.gen r.gen ~]
        [%cnhp *]  [%cncl p.gen q.gen ~]
        ::  this probably should work, but doesn't
        ::
        ::  [%cncl *]  [%cntr [%$ ~] p.gen [[[[%& 6] ~] [%cltr q.gen]] ~]]
        [%cncl *]  [%cnsg [%$ ~] p.gen q.gen]
        [%cnsg *]
      ::  this complex matching system is a leftover from the old
      ::  "electroplating" era.  %cnsg should be removed and replaced
      ::  with the commented-out %cncl above.  but something is broken.
      ::
      :^  %cntr  p.gen  q.gen
      =+  axe=6
      |-  ^-  (list [wing hoon])
      ?~  r.gen  ~
      ?~  t.r.gen  [[[[%| 0 ~] [%& axe] ~] i.r.gen] ~]
      :-  [[[%| 0 ~] [%& (peg axe 2)] ~] i.r.gen]
      $(axe (peg axe 3), r.gen t.r.gen)
    ::
        [%cntr *]
      ?:  =(~ r.gen)
        [%tsgr q.gen [%wing p.gen]]
      :+  %tsls
        q.gen
      :+  %cnts
        (weld p.gen `wing`[[%& 2] ~])
      (turn r.gen |=([p=wing q=hoon] [p [%tsgr [%$ 3] q]]))
    ::
        [%ktdt *]  [%ktls [%cncl p.gen q.gen ~] q.gen]
        [%kthp *]  [%ktls ~(example ax p.gen) q.gen]
        [%ktts *]  (grip(gen q.gen) p.gen)
    ::
        [%sgbr *]
      :+  %sggr
        :-  %mean
        =+  fek=~(feck ap p.gen)
        ?^  fek  [%rock %tas u.fek]
        [%brdt [%cncl [%limb %cain] [%zpgr [%tsgr [%$ 3] p.gen]] ~]]
      q.gen
    ::
        [%sgcb *]  [%sggr [%mean [%brdt p.gen]] q.gen]
        [%sgcn *]
      :+  %sggl
        :-  %fast
        :-  %clls
        :+  [%rock %$ p.gen]
          [%zpts q.gen]
        :-  %clsg
        =+  nob=`(list hoon)`~
        |-  ^-  (list hoon)
        ?~  r.gen
          nob
        [[[%rock %$ p.i.r.gen] [%zpts q.i.r.gen]] $(r.gen t.r.gen)]
      s.gen
    ::
        [%sgfs *]  [%sgcn p.gen [%$ 7] ~ q.gen]
        [%sggl *]  [%tsgl [%sggr p.gen [%$ 1]] q.gen]
        [%sgbc *]  [%sggr [%live [%rock %$ p.gen]] q.gen]
        [%sgls *]  [%sggr [%memo %rock %$ p.gen] q.gen]
        [%sgpm *]
      :+  %sggr
        [%slog [%sand %$ p.gen] [%cncl [%limb %cain] [%zpgr q.gen] ~]]
      r.gen
    ::
        [%sgts *]  [%sggr [%germ p.gen] q.gen]
        [%sgwt *]
      :+  %tsls  [%wtdt q.gen [%bust %null] [[%bust %null] r.gen]]
      :^  %wtsg  [%& 2]~
        [%tsgr [%$ 3] s.gen]
      [%sgpm p.gen [%$ 5] [%tsgr [%$ 3] s.gen]]
    ::
        [%mcts *]
      |-
      ?~  p.gen  [%bust %null]
      ?-  -.i.p.gen
        ^      [[%xray i.p.gen] $(p.gen t.p.gen)]
        %manx  [p.i.p.gen $(p.gen t.p.gen)]
        %tape  [[%mcfs p.i.p.gen] $(p.gen t.p.gen)]
        %call  [%cncl p.i.p.gen [$(p.gen t.p.gen)]~]
        %marl  =-  [%cndt [p.i.p.gen $(p.gen t.p.gen)] -]
               ^-  hoon
               :+  %tsbr  [%base %cell]
               :+  %brpt  ~
               ^-  (map term tome)
               =-  [[%$ ~ -] ~ ~]
               ^-  (map term hoon)
               :_  [~ ~]
               =+  sug=[[%& 12] ~]
               :-  %$
               :^  %wtsg  sug
                 [%cnts sug [[[[%& 1] ~] [%$ 13]] ~]]
               [%cnts sug [[[[%& 3] ~] [%cnts [%$ ~] [[sug [%$ 25]] ~]]] ~]]
      ==
    ::
        [%mccl *]
      ?-    q.gen
          ~      [%zpzp ~]
          [* ~]  i.q.gen
          ^
        :+  %tsls
          p.gen
        =+  yex=`(list hoon)`q.gen
        |-  ^-  hoon
        ?-  yex
          [* ~]  [%tsgr [%$ 3] i.yex]
          [* ^]   [%cncl [%$ 2] [%tsgr [%$ 3] i.yex] $(yex t.yex) ~]
          ~      !!
        ==
      ==
    ::
        [%mcfs *]  =+(zoy=[%rock %ta %$] [%clsg [zoy [%clsg [zoy p.gen] ~]] ~])
        [%mcgl *]  [%cnls [%cnhp q ktcl+p] r [%brts p [%tsgr $+3 s]]]:gen
        [%mcgr *]  [%cnls [%cnhp [%limb %tsgr %m %bind] ktcl+p] [%zpgl p q] [%brts p [%tsgr $+3 r]]]:gen
    ::
        [%mcsg *]                                       ::                  ;~
      |-  ^-  hoon
      ?-  q.gen
          ~      ~_(leaf+"open-mcsg" !!)
          ^
        :+  %tsgr  [%ktts %v %$ 1]                      ::  =>  v=.
        |-  ^-  hoon                                    ::
        ?:  ?=(~ t.q.gen)                               ::
          [%tsgr [%limb %v] i.q.gen]                    ::  =>(v {i.q.gen})
        :+  %tsls  [%ktts %a $(q.gen t.q.gen)]          ::  =+  ^=  a
        :+  %tsls                                       ::    {$(q.gen t.q.gen)}
          [%ktts %b [%tsgr [%limb %v] i.q.gen]]         ::  =+  ^=  b
        :+  %tsls                                       ::    =>(v {i.q.gen})
          :+  %ktts  %c                                 ::  =+  c=,.+6.b
          :+  %tsgl                                     ::
            [%wing [%| 0 ~] [%& 6] ~]                   ::
          [%limb %b]                                    ::
        :-  %brdt                                       ::  |.
        :^    %cnls                                     ::  %+
            [%tsgr [%limb %v] p.gen]                    ::      =>(v {p.gen})
          [%cncl [%limb %b] [%limb %c] ~]               ::    (b c)
        :+  %cnts  [%a ~]                               ::  a(,.+6 c)
        [[[[%| 0 ~] [%& 6] ~] [%limb %c]] ~]            ::
      ==                                                ::
    ::
        [%mcmc *]                                       ::                  ;;
      [%cnhp ~(factory ax p.gen) q.gen]
    ::
        [%tsbr *]
      [%tsls ~(example ax p.gen) q.gen]
    ::
        [%tstr *]
      :+  %tsgl
        r.gen
      [%tune [[p.p.gen ~ ?~(q.p.gen q.gen [%kthp u.q.p.gen q.gen])] ~ ~] ~]
    ::
        [%tscl *]
      [%tsgr [%cncb [[%& 1] ~] p.gen] q.gen]
    ::
        [%tsfs *]
      [%tsls [%ktts p.gen q.gen] r.gen]
    ::
        [%tsmc *]  [%tsfs p.gen r.gen q.gen]
        [%tsdt *]
      [%tsgr [%cncb [[%& 1] ~] [[p.gen q.gen] ~]] r.gen]
        [%tswt *]                                       ::                  =?
      [%tsdt p.gen [%wtcl q.gen r.gen [%wing p.gen]] s.gen]
    ::
        [%tskt *]                                       ::                  =^
      =+  wuy=(weld q.gen `wing`[%v ~])                 ::
      :+  %tsgr  [%ktts %v %$ 1]                        ::  =>  v=.
      :+  %tsls  [%ktts %a %tsgr [%limb %v] r.gen]      ::  =+  a==>(v \r.gen)
      :^  %tsdt  wuy  [%tsgl [%$ 3] [%limb %a]]
      :+  %tsgr  :-  :+  %ktts  [%over [%v ~] p.gen]
                     [%tsgl [%$ 2] [%limb %a]]
                 [%limb %v]
      s.gen
    ::
        [%tsgl *]  [%tsgr q.gen p.gen]
        [%tsls *]  [%tsgr [p.gen [%$ 1]] q.gen]
        [%tshp *]  [%tsls q.gen p.gen]
        [%tssg *]
      |-  ^-  hoon
      ?~  p.gen    [%$ 1]
      ?~  t.p.gen  i.p.gen
      [%tsgr i.p.gen $(p.gen t.p.gen)]
    ::
        [%wtbr *]
      |-
      ?~(p.gen [%rock %f 1] [%wtcl i.p.gen [%rock %f 0] $(p.gen t.p.gen)])
    ::
        [%wtdt *]   [%wtcl p.gen r.gen q.gen]
        [%wtgl *]   [%wtcl p.gen [%zpzp ~] q.gen]
        [%wtgr *]   [%wtcl p.gen q.gen [%zpzp ~]]
        [%wtkt *]   [%wtcl [%wtts [%base %atom %$] p.gen] r.gen q.gen]
    ::
        [%wthp *]
      |-
      ?~  q.gen
        [%lost [%wing p.gen]]
      :^    %wtcl
          [%wtts p.i.q.gen p.gen]
        q.i.q.gen
      $(q.gen t.q.gen)
    ::
        [%wtls *]
      [%wthp p.gen (weld r.gen `_r.gen`[[[%base %noun] q.gen] ~])]
    ::
        [%wtpm *]
      |-
      ?~(p.gen [%rock %f 0] [%wtcl i.p.gen $(p.gen t.p.gen) [%rock %f 1]])
    ::
        [%xray *]
      |^  :-  [(open-mane n.g.p.gen) %clsg (turn a.g.p.gen open-mart)]
          [%mcts c.p.gen]
      ::
      ++  open-mane
        |=  a=mane:hoot
        ?@(a [%rock %tas a] [[%rock %tas -.a] [%rock %tas +.a]])
      ::
      ++  open-mart
        |=  [n=mane:hoot v=(list beer:hoot)]
        [(open-mane n) %knit v]
      --
    ::
        [%wtpt *]   [%wtcl [%wtts [%base %atom %$] p.gen] q.gen r.gen]
        [%wtsg *]   [%wtcl [%wtts [%base %null] p.gen] q.gen r.gen]
        [%wtts *]   [%fits ~(example ax p.gen) q.gen]
        [%wtzp *]   [%wtcl p.gen [%rock %f 1] [%rock %f 0]]
        [%zpgr *]
      [%cncl [%limb %onan] [%zpmc [%kttr [%bcmc %limb %abel]] p.gen] ~]
    ::
        [%zpwt *]
      ?:  ?:  ?=(@ p.gen)
            (lte hoon-version p.gen)
          &((lte hoon-version p.p.gen) (gte hoon-version q.p.gen))
        q.gen
      ~_(leaf+"hoon-version" !!)
    ::
        *           gen
    ==
  ::
  ++  rake  ~>(%mean.'rake-hoon' (need reek))
  ++  reek
    ^-  (unit wing)
    ?+  gen  ~
      [~ *]        `[[%& p.gen] ~]
      [%limb *]     `[p.gen ~]
      [%wing *]     `p.gen
      [%cnts * ~]  `p.gen
      [%dbug *]     reek(gen q.gen)
    ==
  ++  rusk
    ^-  term
    =+  wig=rake
    ?.  ?=([@ ~] wig)
      ~>(%mean.'rusk-hoon' !!)
    i.wig
  --

::    5d: parser
+|  %parser
::
::  +vang: set +vast params
::
::    bug: debug mode
::    doc: doccord parsing
::    wer: where we are
::
++  vang
  |=  [f=$@(? [bug=? doc=?]) wer=path]
  %*(. vast bug ?@(f f bug.f), doc ?@(f & doc.f), wer wer)
::
++  vast                                                ::  main parsing core
  =+  [bug=`?`| wer=*path doc=`?`&]
  |%
  ++  gash  %+  cook                                    ::  parse path
              |=  a=(list tyke)  ^-  tyke
              ?~(a ~ (weld i.a $(a t.a)))
            (more fas limp)
  ++  gasp  ;~  pose                                    ::  parse =path= etc.
              %+  cook
                |=([a=tyke b=tyke c=tyke] :(weld a b c))
              ;~  plug
                (cook |=(a=(list) (turn a |=(b=* ~))) (star tis))
                (cook |=(a=hoon [[~ a] ~]) hasp)
                (cook |=(a=(list) (turn a |=(b=* ~))) (star tis))
              ==
              (cook |=(a=(list) (turn a |=(b=* ~))) (plus tis))
            ==
  ++  glam  ~+((glue ace))
  ++  hasp  ;~  pose                                    ::  path element
              (ifix [sel ser] wide)
              (stag %cncl (ifix [pal par] (most ace wide)))
              (stag %sand (stag %tas (cold %$ buc)))
              (stag %sand (stag %t qut))
              %+  cook
                |=(a=coin [%sand ?:(?=([~ %tas *] a) %tas %ta) ~(rent co a)])
              nuck:so
            ==
  ++  limp  %+  cook
              |=  [a=(list) b=tyke]
              ?~  a  b
              $(a t.a, b [`[%sand %tas %$] b])
            ;~(plug (star fas) gasp)
  ++  mota  %+  cook
              |=([a=tape b=tape] (rap 3 (weld a b)))
            ;~(plug (star low) (star hig))
  ++  docs
    |%
    ::  +apex: prefix comment. may contain batch comments.
    ::
    ::    when a prefix doccord is parsed, it is possible that there is no +gap
    ::    afterward to be consumed, so we add an additional newline and
    ::    decrement the line number in the `hair` of the parser
    ::
    ::    the reason for this is that the whitespace parsing under +vast seems
    ::    to factor more cleanly this way, at least compared to the variations
    ::    tried without the extra newline. this doesn't mean there isn't a
    ::    better factorization without it, though.
    ++  apex
      ?.  doc  (easy *whit)
      %+  knee  *whit  |.  ~+
      ;~  plug
        |=  tub=nail
        =/  vex
          %.  tub
          %-  star
          %+  cook  |*([[a=* b=*] c=*] [a b c])
          ;~(pfix (punt leap) into ;~(pose larg smol))
        ?~  q.vex  vex
        :-  p=p.vex
        %-  some
        ?~  p.u.q.vex
          [p=~ q=q.u.q.vex]
        :-  p=(malt p.u.q.vex)
        q=`nail`[[(dec p.p.q.u.q.vex) q.p.q.u.q.vex] ['\0a' q.q.u.q.vex]]
      ==
    ::
    ::  +apse: postfix comment.
    ::
    ::    a one line comment at the end of a line (typically starting at column
    ::    57) that attaches to the expression starting at the beginning of the
    ::    current line. does not use a $link.
    ++  apse
      ?.  doc  (easy *whiz)
      %+  knee  *whiz  |.  ~+
      ;~  pose
        ;~(less ;~(plug into step en-link col ace) ;~(pfix into step line))
      ::
        (easy *whiz)
      ==
    ::
    ++  leap                                            ::  whitespace w/o docs
      %+  cold  ~
      ;~  plug
        ;~  pose
          (just '\0a')
          ;~(plug gah ;~(pose gah skip))
          skip
        ==
        (star ;~(pose skip gah))
      ==
    ::
    ::  +smol: 2 aces then summary, 4 aces then paragraphs.
    ++  smol
      ;~  pfix
        step
        ;~  plug
          ;~  plug
            (plus en-link)
            ;~  pose
              (ifix [;~(plug col ace) (just '\0a')] (cook crip (plus prn)))
              (ifix [(star ace) (just '\0a')] (easy *cord))
            ==
          ==
          (rant ;~(pfix step step text))
        ==
      ==
    ::
    ::  +larg: 4 aces then summary, 2 aces then paragraphs.
    ++  larg
      ;~  pfix
        step  step
        ;~  plug
          ;~  sfix
            ;~  plug
              ;~  pose
                ;~(sfix (plus en-link) col ace)
                ;~(less ace (easy *cuff))
              ==
              ;~(less ace (cook crip (plus prn)))
            ==
            (just '\0a')
          ==
          (rant ;~(pfix step teyt))
        ==
      ==
    ::
    ++  rant
      |*  sec=rule
      %-  star
      ;~  pfix
        (ifix [into (just '\0a')] (star ace))
        (plus (ifix [into (just '\0a')] sec))
      ==
    ::
    ++  skip                                            ::  non-doccord comment
      ;~  plug
        col  col
        ;~(less ;~(pose larg smol) ;~(plug (star prn) (just '\0a')))
      ==
    ::
    ++  null  (cold ~ (star ace))
    ++  text  (pick line code)
    ++  teyt  (pick line ;~(pfix step code))
    ++  line  ;~(less ace (cook crip (star prn)))
    ++  code  ;~(pfix step ;~(less ace (cook crip (star prn))))
    ++  step  ;~(plug ace ace)
    ::
    ++  into
      ;~(plug (star ace) col col)
    ::
    ++  en-link
      |=  a=nail  %.  a
      %+  knee  *link  |.  ~+
      %-  stew
      ^.  stet  ^.  limo
      :~  :-  '|'  ;~(pfix bar (stag %chat sym))
          :-  '.'  ;~(pfix dot (stag %frag sym))
          :-  '+'  ;~(pfix lus (stag %funk sym))
          :-  '$'  ;~(pfix buc (stag %plan sym))
          :-  '%'  ;~(pfix cen (stag %cone bisk:so))
      ==
    --
  ::
  ++  clad                                              ::  hoon doccords
    |*  fel=rule
    %+  cook
      |=  [a=whit b=hoon c=whiz]
      =?  b  !=(c *whiz)
        [%note help/`[c]~ b]
      =+  docs=~(tap by bat.a)
      |-
      ?~  docs  b
      $(docs t.docs, b [%note help/i.docs b])
    (seam fel)
  ++  coat                                              ::  spec doccords
    |*  fel=rule
    %+  cook
      |=  [a=whit b=spec c=whiz]
      =?  b  !=(c *whiz)
        [%gist help/`[c]~ b]
      =+  docs=~(tap by bat.a)
      |-
      ?~  docs  b
      $(docs t.docs, b [%gist help/i.docs b])
    (seam fel)
  ++  scye                                              ::  with prefix doccords
    |*  fel=rule
    ;~(pose ;~(plug apex:docs ;~(pfix gap fel)) ;~(plug (easy *whit) fel))
  ++  seam                                              ::  with doccords
    |*  fel=rule
    (scye ;~(plug fel apse:docs))
  ::
  ++  plex                                              ::  reparse static path
    |=  gen=hoon  ^-  (unit path)
    ?:  ?=([%dbug *] gen)                               ::  unwrap %dbug
      $(gen q.gen)
    ?.  ?=([%clsg *] gen)  ~                            ::  require :~ hoon
    %+  reel  p.gen                                     ::  build using elements
    |=  [a=hoon b=_`(unit path)`[~ u=/]]                ::  starting from just /
    ?~  b  ~
    ?.  ?=([%sand ?(%ta %tas) @] a)  ~                  ::  /foo constants
    `[q.a u.b]
  ::
  ++  phax
    |=  ruw=(list (list woof))
    =+  [yun=*(list hoon) cah=*(list @)]
    =+  wod=|=([a=tape b=(list hoon)] ^+(b ?~(a b [[%mcfs %knit (flop a)] b])))
    |-  ^+  yun
    ?~  ruw
      (flop (wod cah yun))
    ?~  i.ruw  $(ruw t.ruw)
    ?@  i.i.ruw
      $(i.ruw t.i.ruw, cah [i.i.ruw cah])
    $(i.ruw t.i.ruw, cah ~, yun [p.i.i.ruw (wod cah yun)])
  ::
  ++  posh
    |=  [pre=(unit tyke) pof=(unit [p=@ud q=tyke])]
    ^-  (unit (list hoon))
    =-  ?^(- - ~&(%posh-fail -))
    =+  wom=(poof wer)
    %+  biff
      ?~  pre  `u=wom
      %+  bind  (poon wom u.pre)
      |=  moz=(list hoon)
      ?~(pof moz (weld moz (slag (lent u.pre) wom)))
    |=  yez=(list hoon)
    ?~  pof  `yez
    =+  zey=(flop yez)
    =+  [moz=(scag p.u.pof zey) gul=(slag p.u.pof zey)]
    =+  zom=(poon (flop moz) q.u.pof)
    ?~(zom ~ `(weld (flop gul) u.zom))
  ::
  ++  poof                                              ::  path -> (list hoon)
    |=(pax=path ^-((list hoon) (turn pax |=(a=@ta [%sand %ta a]))))
  ::
  ::  tyke is =foo== as ~[~ `foo ~ ~]
  ::  interpolate '=' path components
  ++  poon                                              ::  try to replace '='s
    |=  [pag=(list hoon) goo=tyke]                      ::    default to pag
    ^-  (unit (list hoon))                              ::    for null goo's
    ?~  goo  `~                                         ::  keep empty goo
    %+  both                                            ::  otherwise head comes
      ?^(i.goo i.goo ?~(pag ~ `u=i.pag))                ::    from goo or pag
    $(goo t.goo, pag ?~(pag ~ t.pag))                   ::  recurse on tails
  ::
  ++  poor
    %+  sear  posh
    ;~  plug
      (stag ~ gash)
      ;~(pose (stag ~ ;~(pfix cen porc)) (easy ~))
    ==
  ::
  ++  porc
    ;~  plug
      (cook |=(a=(list) (lent a)) (star cen))
      ;~(pfix fas gash)
    ==
  ::
  ++  rump
    %+  sear
      |=  [a=wing b=(unit hoon)]  ^-  (unit hoon)
      ?~(b [~ %wing a] ?.(?=([@ ~] a) ~ [~ [%rock %tas i.a] u.b]))
    ;~(plug rope ;~(pose (stag ~ wede) (easy ~)))
  ::
  ++  rood
    ;~  pfix  fas
      (stag %clsg poor)
    ==
  ::
  ++  reed
    ;~  pfix  fas
      (stag %clsg (more fas stem))
    ==
  ::
  ++  stem
    %+  knee  *hoon  |.  ~+
    %+  cook
      |=  iota=$%([%hoon =hoon] iota)
      ?@  iota  [%rock %tas iota]
      ?:  ?=(%hoon -.iota)  hoon.iota
      [%clhp [%rock %tas -.iota] [%sand iota]]
    |^  %-  stew
      ^.  stet  ^.  limo
      :~  :-  'a'^'z'  ;~  pose
                         (spit (stag %cncl (ifix [pal par] (most ace wide))))
                         (spit (ifix [sel ser] wide))
                         (slot sym)
                       ==
          :-  '$'      (cold %$ buc)
          :-  '0'^'9'  (slot bisk:so)
          :-  '-'      (slot tash:so)
          :-  '.'      ;~(pfix dot zust:so)
          :-  '~'      (slot ;~(pfix sig ;~(pose crub:so (easy [%n ~]))))
          :-  '\''     (stag %t qut)
          :-  '['      (slip (ifix [sel ser] wide))
          :-  '('      (slip (stag %cncl (ifix [pal par] (most ace wide))))
      ==
    ::
    ++  slip  |*(r=rule (stag %hoon r))
    ++  slot  |*(r=rule (sear (soft iota) r))
    ++  spit
      |*  r=rule
      %+  stag  %hoon
      %+  cook
        |*([a=term b=*] `hoon`[%clhp [%rock %tas a] b])
      ;~((glue lus) sym r)
    --
  ::
  ++  rupl
    %+  cook
      |=  [a=? b=(list hoon) c=?]
      ?:  a
        ?:  c
          [%clsg [%clsg b] ~]
        [%clsg b]
      ?:  c
        [%clsg [%cltr b] ~]
      [%cltr b]
    ;~  plug
      ;~  pose
        (cold | (just '['))
        (cold & (jest '~['))
      ==
    ::
      ;~  pose
        (ifix [ace gap] (most gap tall))
        (most ace wide)
      ==
    ::
      ;~  pose
        (cold & (jest ']~'))
        (cold | (just ']'))
      ==
    ==
  ::
  ::
  ++  sail                                              ::  xml template
    |=  in-tall-form=?  =|  lin=?
    |%
    ::
    ++  apex                                            ::  product hoon
      %+  cook
        |=  tum=(each manx:hoot marl:hoot)  ^-  hoon
        ?-  -.tum
          %&  [%xray p.tum]
          %|  [%mcts p.tum]
        ==
      top-level
    ::
    ++  top-level                                       ::  entry-point
      ;~(pfix mic ?:(in-tall-form tall-top wide-top))
    ::
    ++  inline-embed                                    ::  brace interpolation
      %+  cook  |=(a=tuna:hoot a)
      ;~  pose
        ;~(pfix mic bracketed-elem(in-tall-form |))
        ;~(plug tuna-mode sump)
        (stag %tape sump)
      ==
    ::
    ++  script-or-style                                 ::  script or style
      %+  cook  |=(a=marx:hoot a)
      ;~  plug
        ;~(pose (jest %script) (jest %style))
        wide-attrs
      ==
    ::
    ++  tuna-mode                                       ::  xml node(s) kind
      ;~  pose
        (cold %tape hep)
        (cold %manx lus)
        (cold %marl tar)
        (cold %call cen)
      ==
    ::
    ++  wide-top                                        ::  wide outer top
      %+  knee  *(each manx:hoot marl:hoot)  |.  ~+
      ;~  pose
        (stag %| wide-quote)
        (stag %| wide-paren-elems)
        (stag %& ;~(plug tag-head wide-tail))
      ==
    ::
    ++  wide-inner-top                                  ::  wide inner top
      %+  knee  *(each tuna:hoot marl:hoot)  |.  ~+
      ;~  pose
        wide-top
        (stag %& ;~(plug tuna-mode wide))
      ==
    ::
    ++  wide-attrs                                      ::  wide attributes
      %+  cook  |=(a=(unit mart:hoot) (fall a ~))
      %-  punt
      %+  ifix  [pal par]
      %+  more  (jest ', ')
      ;~((glue ace) a-mane hopefully-quote)
    ::
    ++  wide-tail                                       ::  wide elements
      %+  cook  |=(a=marl:hoot a)
      ;~(pose ;~(pfix col wrapped-elems) (cold ~ mic) (easy ~))
    ::
    ++  wide-elems                                      ::  wide elements
      %+  cook  |=(a=marl:hoot a)
      %+  cook  join-tops
      (star ;~(pfix ace wide-inner-top))
    ::
    ++  wide-paren-elems                                ::  wide flow
      %+  cook  |=(a=marl:hoot a)
      %+  cook  join-tops
      (ifix [pal par] (more ace wide-inner-top))
    ::
    ::+|
    ::
    ++  drop-top
      |=  a=(each tuna:hoot marl:hoot)  ^-  marl:hoot
      ?-  -.a
        %&  [p.a]~
        %|  p.a
      ==
    ::
    ++  join-tops
      |=  a=(list (each tuna:hoot marl:hoot))  ^-  marl:hoot
      (zing (turn a drop-top))
    ::
    ::+|
    ::
    ++  wide-quote                                      ::  wide quote
      %+  cook  |=(a=marl:hoot a)
      ;~  pose
        ;~  less  (jest '"""')
          (ifix [doq doq] (cook collapse-chars quote-innards))
        ==
      ::
        %-  inde
        %+  ifix  [(jest '"""\0a') (jest '\0a"""')]
        (cook collapse-chars quote-innards(lin |))
      ==
    ::
    ++  quote-innards                                   ::  wide+tall flow
      %+  cook  |=(a=(list $@(@ tuna:hoot)) a)
      %-  star
      ;~  pose
        ;~(pfix bas ;~(pose (mask "-+*%;\{") bas doq bix:ab))
        inline-embed
        ;~(less bas kel ?:(in-tall-form fail doq) prn)
        ?:(lin fail ;~(less (jest '\0a"""') (just '\0a')))
      ==
    ::
    ++  bracketed-elem                                  ::  bracketed element
      %+  ifix  [kel ker]
      ;~(plug tag-head wide-elems)
    ::
    ++  wrapped-elems                                   ::  wrapped tuna
      %+  cook  |=(a=marl:hoot a)
      ;~  pose
        wide-paren-elems
        (cook |=(@t `marl`[;/((trip +<))]~) qut)
        (cook drop-top wide-top)
      ==
    ::
    ++  a-mane                                          ::  mane as hoon
      %+  cook
        |=  [a=@tas b=(unit @tas)]
        ?~(b a [a u.b])
      ;~  plug
        mixed-case-symbol
        ;~  pose
          %+  stag  ~
            ;~(pfix cab mixed-case-symbol)
          (easy ~)
        ==
      ==
    ::
    ++  en-class
      |=  a=(list [%class p=term])
      ^-  (unit [%class tape])
      ?~  a  ~
      %-  some
      :-  %class
      |-
      %+  welp  (trip p.i.a)
      ?~  t.a  ~
      [' ' $(a t.a)]
    ::
    ++  tag-head                                        ::  tag head
      %+  cook
        |=  [a=mane:hoot b=mart:hoot c=mart:hoot]
        ^-  marx:hoot
        [a (weld b c)]
      ;~  plug
        a-mane
      ::
        %+  cook
          |=  a=(list (unit [term (list beer:hoot)]))
          ^-  (list [term (list beer:hoot)])
          :: discard nulls
          (murn a same)
        ;~  plug
          (punt ;~(plug (cold %id hax) (cook trip sym)))
          (cook en-class (star ;~(plug (cold %class dot) sym)))
          (punt ;~(plug ;~(pose (cold %href fas) (cold %src pat)) soil))
          (easy ~)
        ==
      ::
        wide-attrs
      ==
    ::
    ++  tall-top                                        ::  tall top
      %+  knee  *(each manx:hoot marl:hoot)  |.  ~+
      ;~  pose
        (stag %| ;~(pfix (plus ace) (cook collapse-chars quote-innards)))
        (stag %& ;~(plug script-or-style script-style-tail))
        (stag %& tall-elem)
        (stag %| wide-quote)
        (stag %| ;~(pfix tis tall-tail))
        (stag %& ;~(pfix gar gap (stag [%div ~] cram)))
        (stag %| ;~(plug ;~((glue gap) tuna-mode tall) (easy ~)))
        (easy %| [;/("\0a")]~)
      ==
    ::
    ++  tall-attrs                                      ::  tall attributes
      %-  star
      ;~  pfix  ;~(plug gap tis)
        ;~((glue gap) a-mane hopefully-quote)
      ==
    ::
    ++  tall-elem                                       ::  tall preface
      %+  cook
        |=  [a=[p=mane:hoot q=mart:hoot] b=mart:hoot c=marl:hoot]
        ^-  manx:hoot
        [[p.a (weld q.a b)] c]
      ;~(plug tag-head tall-attrs tall-tail)
    ::
    ::REVIEW is there a better way to do this?
    ++  hopefully-quote                                 :: prefer "quote" form
      %+  cook  |=(a=(list beer:hoot) a)
      %+  cook  |=(a=hoon ?:(?=(%knit -.a) p.a [~ a]~))
      wide
    ::
    ++  script-style-tail                               ::  unescaped tall tail
      %+  cook  |=(a=marl:hoot a)
      %+  ifix  [gap ;~(plug gap duz)]
      %+  most  gap
      ;~  pfix  mic
        %+  cook  |=(a=tape ;/(a))
        ;~  pose
          ;~(pfix ace (star prn))
          (easy "\0a")
        ==
      ==
    ::
    ++  tall-tail                                       ::  tall tail
      ?>  in-tall-form
      %+  cook  |=(a=marl:hoot a)
      ;~  pose
        (cold ~ mic)
        ;~(pfix col wrapped-elems(in-tall-form |))
        ;~(pfix col ace (cook collapse-chars(in-tall-form |) quote-innards))
        (ifix [gap ;~(plug gap duz)] tall-kids)
      ==
    ::
    ++  tall-kids                                       ::  child elements
      %+  cook  join-tops
      ::  look for sail first, or markdown if not
      (most gap ;~(pose top-level (stag %| cram)))
    ::
    ++  collapse-chars                                  ::  group consec chars
      |=  reb=(list $@(@ tuna:hoot))
      ^-  marl:hoot
      =|  [sim=(list @) tuz=marl:hoot]
      |-  ^-  marl:hoot
      ?~  reb
        =.  sim
          ?.  in-tall-form   sim
          [10 |-(?~(sim sim ?:(=(32 i.sim) $(sim t.sim) sim)))]
        ?~(sim tuz [;/((flop sim)) tuz])
      ?@  i.reb
        $(reb t.reb, sim [i.reb sim])
      ?~  sim  [i.reb $(reb t.reb, sim ~)]
      [;/((flop sim)) i.reb $(reb t.reb, sim ~)]
    --
  ++  cram                                              ::  parse unmark
    =>  |%
        ++  item  (pair mite marl:hoot)                 ::  xml node generator
        ++  colm  @ud                                   ::  column
        ++  tarp  marl:hoot                             ::  node or generator
        ++  mite                                        ::  context
          $?  %down                                     ::  outer embed
              %lunt                                     ::  unordered list
              %lime                                     ::  list item
              %lord                                     ::  ordered list
              %poem                                     ::  verse
              %bloc                                     ::  blockquote
              %head                                     ::  heading
          ==                                            ::
        ++  trig                                        ::  line style
          $:  col=@ud                                   ::  start column
              sty=trig-style                            ::  style
          ==                                            ::
        ++  trig-style                                  ::  type of parsed line
          $%  $:  %end                                  ::  terminator
          $?  %done                                     ::  end of input
                  %stet                                 ::    == end of markdown
                  %dent                                 ::    outdent
              ==  ==                                    ::
              $:  %one                                  ::  leaf node
              $?  %rule                                 ::    --- horz rule
                  %fens                                 ::    ``` code fence
                  %expr                                 ::    ;sail expression
              ==  ==                                    ::
              [%new p=trig-new]                         ::  open container
              [%old %text]                              ::  anything else
          ==                                            ::
        ++  trig-new                                    ::  start a
          $?  %lite                                     ::    + line item
              %lint                                     ::    - line item
              %head                                     ::  # heading
              %bloc                                     ::  > block-quote
              %poem                                     ::    [ ]{8} poem
          ==                                            ::
        ++  graf                                        ::  paragraph element
          $%  [%bold p=(list graf)]                     ::  *bold*
              [%talc p=(list graf)]                     ::  _italics_
              [%quod p=(list graf)]                     ::  "double quote"
              [%code p=tape]                            ::  code literal
              [%text p=tape]                            ::  text symbol
              [%link p=(list graf) q=tape]              ::  URL
              [%mage p=tape q=tape]                     ::  image
              [%expr p=tuna:hoot]                       ::  interpolated hoon
          ==
        --
    =<  (non-empty:parse |=(nail `(like tarp)`~($ main +<)))
    |%
    ++  main
      ::
      ::    state of the parsing loop.
      ::
      ::  we maintain a construction stack for elements and a line
      ::  stack for lines in the current block.  a blank line
      ::  causes the current block to be parsed and thrown in the
      ::  current element.  when the indent column retreats, the
      ::  element stack rolls up.
      ::
      ::  .verbose: debug printing enabled
      ::  .err: error position
      ::  .ind: outer and inner indent level
      ::  .hac: stack of items under construction
      ::  .cur: current item under construction
      ::  .par: current "paragraph" being read in
      ::  .[loc txt]: parsing state
      ::
      =/  verbose  &
      =|  err=(unit hair)
      =|  ind=[out=@ud inr=@ud]
      =|  hac=(list item)
      =/  cur=item  [%down ~]
      =|  par=(unit (pair hair wall))
      |_  [loc=hair txt=tape]
      ::
      ++  $                                           ::  resolve
        ^-  (like tarp)
        =>  line
        ::
        ::  if error position is set, produce error
        ?.  =(~ err)
          ~&  err+err
          [+.err ~]
        ::
        ::  all data was consumed
        =-  [loc `[- [loc txt]]]
        =>  close-par
        |-  ^-  tarp
        ::
        ::  fold all the way to top
        ?~  hac  cur-to-tarp
        $(..^$ close-item)
      ::
      ::+|
      ::
      ++  cur-indent
        ?-  p.cur
          %down  2
          %head  0
          %lunt  0
          %lime  2
          %lord  0
          %poem  8
          %bloc  2
        ==
      ::
      ++  back                                          ::  column retreat
        |=  luc=@ud
        ^+  +>
        ?:  (gte luc inr.ind)  +>
        ::
        ::  nex: next backward step that terminates this context
        =/  nex=@ud  cur-indent  ::  REVIEW code and poem blocks are
                                 ::  handled elsewhere
        ?:  (gth nex (sub inr.ind luc))
          ::
          ::  indenting pattern violation
          ~?  verbose  indent-pattern-violation+[p.cur nex inr.ind luc]
          ..^$(inr.ind luc, err `[p.loc luc])
        =.  ..^$  close-item
        $(inr.ind (sub inr.ind nex))
      ::
      ++  cur-to-tarp                                   ::  item to tarp
        ^-  tarp
        ?:  ?=(?(%down %head %expr) p.cur)
          (flop q.cur)
        =-  [[- ~] (flop q.cur)]~
        ?-  p.cur
          %lunt  %ul
          %lord  %ol
          %lime  %li
          %poem  %div ::REVIEW actual container element?
          %bloc  %blockquote
        ==
      ::
      ++  close-item  ^+  .                             ::  complete and pop
        ?~  hac  .
        %=  .
          hac  t.hac
          cur  [p.i.hac (weld cur-to-tarp q.i.hac)]
        ==
      ::
      ++  read-line                                     ::  capture raw line
        =|  lin=tape
        |-  ^+  [[lin *(unit _err)] +<.^$]  :: parsed tape and halt/error
        ::
        ::  no unterminated lines
        ?~  txt
          ~?  verbose  %unterminated-line
          [[~ ``loc] +<.^$]
        ?.  =(`@`10 i.txt)
          ?:  (gth inr.ind q.loc)
            ?.  =(' ' i.txt)
              ~?  verbose  expected-indent+[inr.ind loc txt]
              [[~ ``loc] +<.^$]
            $(txt t.txt, q.loc +(q.loc))
          ::
          ::  save byte and repeat
          $(txt t.txt, q.loc +(q.loc), lin [i.txt lin])
        =.  lin
        ::
        ::  trim trailing spaces
        |-  ^-  tape
          ?:  ?=([%' ' *] lin)
            $(lin t.lin)
          (flop lin)
          ::
        =/  eat-newline=nail  [[+(p.loc) 1] t.txt]
        =/  saw  look(+<.$ eat-newline)
        ::
        ?:  ?=([~ @ %end ?(%stet %dent)] saw)           ::  stop on == or dedent
          [[lin `~] +<.^$]
        [[lin ~] eat-newline]
      ::
      ++  look                                          ::  inspect line
        ^-  (unit trig)
        %+  bind  (wonk (look:parse loc txt))
        |=  a=trig  ^+  a
        ::
        ::  treat a non-terminator as a terminator
        ::  if it's outdented
        ?:  =(%end -.sty.a)  a
        ?:  (lth col.a out.ind)
          a(sty [%end %dent])
        a
      ::
      ++  close-par                                     ::  make block
        ^+  .
        ::
        ::  empty block, no action
        ?~  par  .
        ::
        ::  if block is verse
        ?:  ?=(%poem p.cur)
          ::
          ::  add break between stanzas
          =.  q.cur  ?~(q.cur q.cur [[[%br ~] ~] q.cur])
          =-  close-item(par ~, q.cur (weld - q.cur), inr.ind (sub inr.ind 8))
          %+  turn  q.u.par
          |=  tape  ^-  manx
          ::
          ::  each line is a paragraph
          :-  [%p ~]
          :_  ~
          ;/("{+<}\0a")
        ::
        ::  yex: block recomposed, with newlines
        =/  yex=tape
          %-  zing
          %+  turn  (flop q.u.par)
          |=  a=tape
          (runt [(dec inr.ind) ' '] "{a}\0a")
        ::
        ::  vex: parse of paragraph
        =/  vex=(like tarp)
          ::
          ::  either a one-line header or a paragraph
          %.  [p.u.par yex]
          ?:  ?=(%head p.cur)
            (full head:parse)
          (full para:parse)
        ::
        ::  if error, propagate correctly
        ?~  q.vex
          ~?  verbose  [%close-par p.cur yex]
          ..$(err `p.vex)
        ::
        ::  finish tag if it's a header
        =<  ?:(?=(%head p.cur) close-item ..$)
        ::
        ::  save good result, clear buffer
        ..$(par ~, q.cur (weld p.u.q.vex q.cur))
      ::
      ++  line  ^+  .                                   ::  body line loop
        ::
        ::  abort after first error
        ?:  !=(~ err)  .
        ::
        ::  saw: profile of this line
        =/  saw  look
        ~?  [debug=|]  [%look ind=ind saw=saw txt=txt]
        ::
        ::  if line is blank
        ?~  saw
          ::
          ::  break section
          =^  a=[tape fin=(unit _err)]  +<.$  read-line
          ?^  fin.a
            ..$(err u.fin.a)
          =>(close-par line)
        ::
        ::  line is not blank
        =>  .(saw u.saw)
        ::
        ::  if end of input, complete
        ?:  ?=(%end -.sty.saw)
          ..$(q.loc col.saw)
        ::
        =.  ind  ?~(out.ind [col.saw col.saw] ind)      ::  init indents
        ::
        ?:  ?|  ?=(~ par)                          :: if after a paragraph or
                ?&  ?=(?(%down %lime %bloc) p.cur)  :: unspaced new container
                    |(!=(%old -.sty.saw) (gth col.saw inr.ind))
            ==  ==
          =>  .(..$ close-par)
            ::
          ::  if column has retreated, adjust stack
          =.  ..$  (back col.saw)
            ::
          =^  col-ok  sty.saw
            ?+  (sub col.saw inr.ind)  [| sty.saw]      :: columns advanced
              %0  [& sty.saw]
              %8  [& %new %poem]
            ==
          ?.  col-ok
            ~?  verbose  [%columns-advanced col.saw inr.ind]
            ..$(err `[p.loc col.saw])
        ::
          =.  inr.ind  col.saw
      ::
          ::  unless adding a matching item, close lists
          =.  ..$
            ?:  ?|  &(?=(%lunt p.cur) !?=(%lint +.sty.saw))
                    &(?=(%lord p.cur) !?=(%lite +.sty.saw))
                ==
              close-item
            ..$
        ::
          =<  line(par `[loc ~])  ^+  ..$               ::  continue with para
          ?-    -.sty.saw
              %one  (read-one +.sty.saw)                ::  parse leaves
              %new  (open-item p.sty.saw)               ::  open containers
              %old  ..$                                 ::  just text
          ==
        ::
        ::
        ::- - - foo
        ::  detect bad block structure
        ?.  ::  first line of container is legal
            ?~  q.u.par  &
            ?-  p.cur
        ::
            ::  can't(/directly) contain text
              ?(%lord %lunt)  ~|(bad-leaf-container+p.cur !!)
        ::
            ::  only one line in a header
              %head  |
          ::
            ::  indented literals need to end with a blank line
              %poem  (gte col.saw inr.ind)
        ::
            ::  text tarps must continue aligned
              ?(%down %lunt %lime %lord %bloc)  =(col.saw inr.ind)
          ==
          ~?  verbose  bad-block-structure+[p.cur inr.ind col.saw]
          ..$(err `[p.loc col.saw])
        ::
        ::  accept line and maybe continue
        =^  a=[lin=tape fin=(unit _err)]  +<.$  read-line
        =.  par  par(q.u [lin.a q.u.par])
        ?^  fin.a  ..$(err u.fin.a)
        line
      ++  parse-block                                   ::  execute parser
        |=  fel=$-(nail (like tarp))  ^+  +>
        =/  vex=(like tarp)  (fel loc txt)
        ?~  q.vex
          ~?  verbose  [%parse-block txt]
          +>.$(err `p.vex)
        =+  [res loc txt]=u.q.vex
        %_  +>.$
          loc  loc
          txt  txt
          q.cur  (weld (flop `tarp`res) q.cur)          ::  prepend to the stack
        ==
      ::
      ++  read-one                                      ::  read %one item
        |=  sty=?(%expr %rule %fens)  ^+  +>
        ?-  sty
          %expr  (parse-block expr:parse)
          %rule  (parse-block hrul:parse)
          %fens  (parse-block (fens:parse inr.ind))
        ==
      ::
      ++  open-item                                     ::  enter list/quote
        |=  saw=trig-new
        =<  +>.$:apex
        |%
        ++  apex  ^+  .                                 ::  open container
          ?-  saw
            %poem  (push %poem)                         ::  verse literal
            %head  (push %head)                         ::  heading
            %bloc  (entr %bloc)                         ::  blockquote line
            %lint  (lent %lunt)                         ::  unordered list
            %lite  (lent %lord)                         ::  ordered list
          ==
        ::
        ++  push                                        ::  push context
          |=(mite +>(hac [cur hac], cur [+< ~]))
        ::
        ++  entr                                        ::  enter container
          |=  typ=mite
          ^+  +>
          ::
          ::  indent by 2
          =.  inr.ind  (add 2 inr.ind)
          ::
          ::  "parse" marker
          =.  txt  (slag (sub inr.ind q.loc) txt)
          =.  q.loc  inr.ind
          ::
          (push typ)
        ::
        ++  lent                                        ::  list entry
          |=  ord=?(%lord %lunt)
          ^+  +>
          =>  ?:(=(ord p.cur) +>.$ (push ord))          ::  push list if new
          (entr %lime)
        --
      --
    ::
    ++  parse                                           ::  individual parsers
      |%
      ++  look                                          ::  classify line
        %+  cook  |=(a=(unit trig) a)
        ;~  pfix  (star ace)
          %+  here                                      ::  report indent
            |=([a=pint b=?(~ trig-style)] ?~(b ~ `[q.p.a b]))
          ;~  pose
            (cold ~ (just `@`10))                       ::  blank line
          ::
            (full (easy [%end %done]))                  ::  end of input
            (cold [%end %stet] duz)                     ::  == end of markdown
          ::
            (cold [%one %rule] ;~(plug hep hep hep))    ::  --- horizontal ruler
            (cold [%one %fens] ;~(plug tic tic tic))    ::  ``` code fence
            (cold [%one %expr] mic)                     ::  ;sail expression
          ::
            (cold [%new %head] ;~(plug (star hax) ace)) ::  # heading
            (cold [%new %lint] ;~(plug hep ace))        ::  - line item
            (cold [%new %lite] ;~(plug lus ace))        ::  + line item
            (cold [%new %bloc] ;~(plug gar ace))        ::  > block-quote
          ::
            (easy [%old %text])                         ::  anything else
          ==
        ==
      ::
      ::
      ++  calf                                          ::  cash but for tic tic
        |*  tem=rule
        %-  star
        ;~  pose
          ;~(pfix bas tem)
          ;~(less tem prn)
        ==
      ++  cash                                          ::  escaped fence
        |*  tem=rule
        %-  echo
        %-  star
        ;~  pose
          whit
          ;~(plug bas tem)
          ;~(less tem prn)
        ==
      ::
      ++  cool                                          ::  reparse
        |*  $:  ::  fex: primary parser
                ::  sab: secondary parser
                ::
                fex=rule
                sab=rule
            ==
        |=  [loc=hair txt=tape]
        ^+  *sab
        ::
        ::  vex: fenced span
        =/  vex=(like tape)  (fex loc txt)
        ?~  q.vex  vex
        ::
        ::  hav: reparse full fenced text
        =/  hav  ((full sab) [loc p.u.q.vex])
        ::
        ::  reparsed error position is always at start
        ?~  q.hav  [loc ~]
        ::
        ::  the complete type with the main product
        :-  p.vex
        `[p.u.q.hav q.u.q.vex]
      ::
      ::REVIEW surely there is a less hacky "first or after space" solution
      ++  easy-sol                                      ::  parse start of line
        |*  a=*
        |=  b=nail
        ?:  =(1 q.p.b)  ((easy a) b)
        (fail b)
      ::
      ++  echo                                          ::  hoon literal
        |*  sab=rule
        |=  [loc=hair txt=tape]
        ^-  (like tape)
        ::
        ::  vex: result of parsing wide hoon
        =/  vex  (sab loc txt)
        ::
        ::  use result of expression parser
        ?~  q.vex  vex
        =-  [p.vex `[- q.u.q.vex]]
        ::
        ::  but replace payload with bytes consumed
        |-  ^-  tape
        ?:  =(q.q.u.q.vex txt)  ~
        ?~  txt  ~
        [i.txt $(txt +.txt)]
      ::
      ++  non-empty
        |*  a=rule
        |=  tub=nail  ^+  (a)
        =/  vex  (a tub)
        ~!  vex
        ?~  q.vex  vex
        ?.  =(tub q.u.q.vex)  vex
        (fail tub)
      ::
      ::
      ++  word                                          ::  tarp parser
        %+  knee  *(list graf)  |.  ~+
        %+  cook
          |=  a=$%(graf [%list (list graf)])
          ^-  (list graf)
          ?:(?=(%list -.a) +.a [a ~])
        ;~  pose
        ::
        ::  ordinary word
        ::
          %+  stag  %text
          ;~(plug ;~(pose low hig) (star ;~(pose nud low hig hep)))
        ::
        ::  naked \escape
        ::
          (stag %text ;~(pfix bas (cook trip ;~(less ace prn))))
        ::
        ::  trailing \ to add <br>
        ::
          (stag %expr (cold [[%br ~] ~] ;~(plug bas (just '\0a'))))
        ::
        ::  *bold literal*
        ::
          (stag %bold (ifix [tar tar] (cool (cash tar) werk)))
        ::
        ::  _italic literal_
        ::
          (stag %talc (ifix [cab cab] (cool (cash cab) werk)))
        ::
        ::  "quoted text"
        ::
          (stag %quod (ifix [doq doq] (cool (cash doq) werk)))
        ::
        ::  `classic markdown quote`
        ::
          (stag %code (ifix [tic tic] (calf tic)))
        ::
        ::  ++arm, +$arm, +*arm, ++arm:core, ...
        ::
          %+  stag  %code
          ;~  plug
            lus  ;~(pose lus buc tar)
            low  (star ;~(pose nud low hep col))
          ==
        ::
        ::  [arbitrary *content*](url)
        ::
          %+  stag  %link
          ;~  (glue (punt whit))
            (ifix [sel ser] (cool (cash ser) werk))
            (ifix [pal par] (cash par))
          ==
        ::
        ::  ![alt text](url)
        ::
          %+  stag  %mage
          ;~  pfix  zap
            ;~  (glue (punt whit))
              (ifix [sel ser] (cash ser))
              (ifix [pal par] (cash par))
            ==
          ==
        ::
        ::  #hoon
        ::
          %+  stag  %list
          ;~  plug
            (stag %text ;~(pose (cold " " whit) (easy-sol ~)))
            (stag %code ;~(pfix hax (echo wide)))
            ;~(simu whit (easy ~))
          ==
        ::
        ::  direct hoon constant
        ::
          %+  stag  %list
          ;~  plug
            (stag %text ;~(pose (cold " " whit) (easy-sol ~)))
          ::
            %+  stag  %code
            %-  echo
            ;~  pose
              ::REVIEW just copy in 0x... parsers directly?
              ;~(simu ;~(plug (just '0') alp) bisk:so)
            ::
              tash:so
              ;~(pfix dot perd:so)
              ;~(pfix sig ;~(pose twid:so (easy [%$ %n 0])))
              ;~(pfix cen ;~(pose sym buc pam bar qut nuck:so))
            ==
          ::
            ;~(simu whit (easy ~))
          ==
        ::
        ::  whitespace
        ::
          (stag %text (cold " " whit))
        ::
        ::  {interpolated} sail
        ::
          (stag %expr inline-embed:(sail |))
        ::
        ::  just a byte
        ::
          (stag %text (cook trip ;~(less ace prn)))
        ==
      ::
      ++  werk  (cook zing (star word))                 ::  indefinite tarp
      ::
      ++  down                                          ::  parse inline tarp
        %+  knee  *tarp  |.  ~+
        =-  (cook - werk)
        ::
        ::  collect raw tarp into xml tags
        |=  gaf=(list graf)
        ^-  tarp
        =<  main
        |%
        ++  main
          ^-  tarp
          ?~  gaf  ~
          ?.  ?=(%text -.i.gaf)
            (weld (item i.gaf) $(gaf t.gaf))
          ::
          ::  fip: accumulate text blocks
          =/  fip=(list tape)  [p.i.gaf]~
          |-  ^-  tarp
          ?~  t.gaf  [;/((zing (flop fip))) ~]
          ?.  ?=(%text -.i.t.gaf)
            [;/((zing (flop fip))) ^$(gaf t.gaf)]
          $(gaf t.gaf, fip :_(fip p.i.t.gaf))
        ::
        ++  item
          |=  nex=graf
          ^-  tarp  ::CHECK can be tuna:hoot?
          ?-  -.nex
            %text  !!  :: handled separately
            %expr  [p.nex]~
            %bold  [[%b ~] ^$(gaf p.nex)]~
            %talc  [[%i ~] ^$(gaf p.nex)]~
            %code  [[%code ~] ;/(p.nex) ~]~
            %quod  ::
                   ::  smart quotes
                   %=    ^$
                       gaf
                     :-  [%text (tufa ~-~201c. ~)]
                     %+  weld  p.nex
                     `(list graf)`[%text (tufa ~-~201d. ~)]~
                   ==
            %link  [[%a [%href q.nex] ~] ^$(gaf p.nex)]~
            %mage  [[%img [%src q.nex] ?~(p.nex ~ [%alt p.nex]~)] ~]~
          ==
        --
      ::
      ++  hrul                                          ::  empty besides fence
        %+  cold  [[%hr ~] ~]~
        ;~(plug (star ace) hep hep hep (star hep) (just '\0a'))
      ::
      ++  tics
        ;~(plug tic tic tic (just '\0a'))
      ::
      ++  fens
        |=  col=@u  ~+
        =/  ind  (stun [(dec col) (dec col)] ace)
        =/  ind-tics  ;~(plug ind tics)
        %+  cook  |=(txt=tape `tarp`[[%pre ~] ;/(txt) ~]~)
        ::
        ::  leading outdent is ok since container may
        ::  have already been parsed and consumed
        %+  ifix  [;~(plug (star ace) tics) ind-tics]
        %^  stir  ""  |=([a=tape b=tape] "{a}\0a{b}")
        ;~  pose
          %+  ifix  [ind (just '\0a')]
          ;~(less tics (star prn))
        ::
          (cold "" ;~(plug (star ace) (just '\0a')))
        ==
      ::
      ++  para                                          ::  paragraph
        %+  cook
          |=(a=tarp ?~(a ~ [[%p ~] a]~))
        ;~(pfix (punt whit) down)
      ::
      ++  expr                                          ::  expression
        =>  (sail &)                                    ::  tall-form
        %+  ifix  [(star ace) ;~(simu gap (easy))]      ::  look-ahead for gap
        (cook drop-top top-level)                       ::  list of tags
        ::
      ::
      ++  whit                                          ::  whitespace
        (cold ' ' (plus ;~(pose (just ' ') (just '\0a'))))
      ::
      ++  head                                          ::  parse heading
        %+  cook
          |=  [haxes=tape kids=tarp]  ^-  tarp
          =/  tag  (crip 'h' <(lent haxes)>)            ::  e.g. ### -> %h3
          =/  id  (contents-to-id kids)
          [[tag [%id id]~] kids]~
        ::
        ;~(pfix (star ace) ;~((glue whit) (stun [1 6] hax) down))
      ::
      ++  contents-to-id                                ::  # text into elem id
        |=  a=(list tuna:hoot)  ^-  tape
        =;  raw=tape
          %+  turn  raw
          |=  @tD
          ^-  @tD
          ?:  ?|  &((gte +< 'a') (lte +< 'z'))
                  &((gte +< '0') (lte +< '9'))
              ==
            +<
          ?:  &((gte +< 'A') (lte +< 'Z'))
            (add 32 +<)
          '-'
        ::
        ::  collect all text in header tarp
        |-  ^-  tape
        ?~  a  ~
        %+  weld
          ^-  tape
          ?-    i.a
              [[%$ [%$ *] ~] ~]                       ::  text node contents
            (murn v.i.a.g.i.a |=(a=beer:hoot ?^(a ~ (some a))))
              [^ *]  $(a c.i.a)                         ::  concatenate children
              [@ *]  ~                                  ::  ignore interpolation
          ==
        $(a t.a)
      --
    --
  ::
  ++  scad
    %+  knee  *spec  |.  ~+
    %-  stew
    ^.  stet  ^.  limo
    :~
      :-  '_'
        ;~(pfix cab (stag %bccb wide))
      :-  ','
        ;~(pfix com (stag %bcmc wide))
      :-  '$'
        (stag %like (most col rope))
      :-  '%'
        ;~  pose
          ;~  pfix  cen
            ;~  pose
              (stag %leaf (stag %tas (cold %$ buc)))
              (stag %leaf (stag %f (cold & pam)))
              (stag %leaf (stag %f (cold | bar)))
              (stag %leaf (stag %t qut))
              (stag %leaf (sear |=(a=coin ?:(?=(%$ -.a) (some +.a) ~)) nuck:so))
            ==
          ==
        ==
      :-  '('
        %+  cook  |=(spec +<)
        %+  stag  %make
        %+  ifix  [pal par]
        ;~  plug
          wide
          ;~(pose ;~(pfix ace (most ace wyde)) (easy ~))
        ==
      :-  '['
        (stag %bccl (ifix [sel ser] (most ace wyde)))
      :-  '*'
        (cold [%base %noun] tar)
      :-  '/'
        ;~(pfix fas (stag %loop ;~(pose (cold %$ buc) sym)))
      :-  '@'
        ;~(pfix pat (stag %base (stag %atom mota)))
      :-  '?'
        ;~  pose
          %+  stag  %bcwt
          ;~(pfix wut (ifix [pal par] (most ace wyde)))
        ::
          (cold [%base %flag] wut)
        ==
      :-  '~'
        (cold [%base %null] sig)
      :-  '!'
        (cold [%base %void] ;~(plug zap zap))
      :-  '^'
        ;~  pose
          (stag %like (most col rope))
          (cold [%base %cell] ket)
        ==
      :-  '='
        ;~  pfix  tis
          %+  sear
            |=  [=(unit term) =spec]
            %+  bind
              ~(autoname ax spec)
            |=  =term
            =*  name  ?~(unit term (cat 3 u.unit (cat 3 '-' term)))
            [%bcts name spec]
          ;~  pose
            ;~(plug (stag ~ ;~(sfix sym tis)) wyde)
            (stag ~ wyde)
          ==
        ==
      :-  ['a' 'z']
        ;~  pose
          (stag %bcts ;~(plug sym ;~(pfix tis wyde)))
          (stag %like (most col rope))
        ==
    ==
  ::
  ++  scat
    %+  knee  *hoon  |.  ~+
    %-  stew
    ^.  stet  ^.  limo
    :~
      :-  ','
        ;~  pose
          (stag %ktcl ;~(pfix com wyde))
          (stag %wing rope)
        ==
      :-  '!'
        ;~  pose
          (stag %wtzp ;~(pfix zap wide))
          (stag %zpzp (cold ~ ;~(plug zap zap)))
        ==
      :-  '_'
        ;~(pfix cab (stag %ktcl (stag %bccb wide)))
      :-  '$'
        ;~  pose
          ;~  pfix  buc
            ;~  pose
              ::  XX: these are all obsolete in hoon 142
              ::
              (stag %leaf (stag %tas (cold %$ buc)))
              (stag %leaf (stag %t qut))
              (stag %leaf (sear |=(a=coin ?:(?=(%$ -.a) (some +.a) ~)) nuck:so))
            ==
          ==
          rump
        ==
      :-  '%'
        ;~  pfix  cen
          ;~  pose
            (stag %clsg (sear |~([a=@ud b=tyke] (posh ~ ~ a b)) porc))
            (stag %rock (stag %tas (cold %$ buc)))
            (stag %rock (stag %f (cold & pam)))
            (stag %rock (stag %f (cold | bar)))
            (stag %rock (stag %t qut))
            (cook (jock &) nuck:so)
            (stag %clsg (sear |=(a=(list) (posh ~ ~ (lent a) ~)) (star cen)))
          ==
        ==
      :-  '&'
        ;~  pose
          (cook |=(a=wing [%cnts a ~]) rope)
          (stag %wtpm ;~(pfix pam (ifix [pal par] (most ace wide))))
          ;~(plug (stag %rock (stag %f (cold & pam))) wede)
          (stag %sand (stag %f (cold & pam)))
        ==
      :-  '\''
        (stag %sand (stag %t qut))
      :-  '('
        (stag %cncl (ifix [pal par] (most ace wide)))
      :-  '*'
        ;~  pose
          (stag %kttr ;~(pfix tar wyde))
          (cold [%base %noun] tar)
        ==
      :-  '@'
        ;~(pfix pat (stag %base (stag %atom mota)))
      :-  '+'
        ;~  pose
          (stag %dtls ;~(pfix lus (ifix [pal par] wide)))
        ::
          %+  cook
            |=  a=(list (list woof))
            :-  %mcfs
            [%knit |-(^-((list woof) ?~(a ~ (weld i.a $(a t.a)))))]
          (most dog ;~(pfix lus soil))
        ::
          (cook |=(a=wing [%cnts a ~]) rope)
        ==
      :-  '-'
        ;~  pose
          (stag %sand tash:so)
        ::
          %+  cook
            |=  a=(list (list woof))
            [%clsg (phax a)]
          (most dog ;~(pfix hep soil))
        ::
          (cook |=(a=wing [%cnts a ~]) rope)
        ==
      :-  '.'
        ;~  pose
          (cook (jock |) ;~(pfix dot perd:so))
          (cook |=(a=wing [%cnts a ~]) rope)
        ==
      :-  ['0' '9']
        %+  cook
          |=  [a=dime b=(unit hoon)]
          ?~(b [%sand a] [[%rock a] u.b])
        ;~(plug bisk:so (punt wede))
      :-  ':'
        ;~  pfix  col
          ;~  pose
            (stag %mccl (ifix [pal par] (most ace wide)))
            ;~(pfix fas (stag %mcfs wide))
          ==
        ==
      :-  '='
        ;~  pfix  tis
          ;~  pose
            (stag %dtts (ifix [pal par] ;~(glam wide wide)))
          ::
            %+  sear
              ::  mainly used for +skin formation
              ::
              |=  =spec
              ^-  (unit hoon)
              %+  bind  ~(autoname ax spec)
              |=(=term `hoon`[%ktts term %kttr spec])
            wyde
          ==
        ==
      :-  '?'
        ;~  pose
          %+  stag  %ktcl
          (stag %bcwt ;~(pfix wut (ifix [pal par] (most ace wyde))))
        ::
          (cold [%base %flag] wut)
        ==
      :-  '['
        rupl
      :-  '^'
        ;~  pose
          (stag %wing rope)
          (cold [%base %cell] ket)
        ==
      :-  '`'
        ;~  pfix  tic
          ;~  pose
            %+  cook
              |=([a=@ta b=hoon] [%ktls [%sand a 0] [%ktls [%sand %$ 0] b]])
            ;~(pfix pat ;~(plug mota ;~(pfix tic wide)))
            ;~  pfix  tar
              (stag %kthp (stag [%base %noun] ;~(pfix tic wide)))
            ==
            (stag %kthp ;~(plug wyde ;~(pfix tic wide)))
            (stag %ktls ;~(pfix lus ;~(plug wide ;~(pfix tic wide))))
            (cook |=(a=hoon [[%rock %n ~] a]) wide)
          ==
        ==
      :-  '"'
        %+  cook
          |=  a=(list (list woof))
          [%knit |-(^-((list woof) ?~(a ~ (weld i.a $(a t.a)))))]
        (most dog soil)
      :-  ['a' 'z']
        rump
      :-  '|'
        ;~  pose
          (cook |=(a=wing [%cnts a ~]) rope)
          (stag %wtbr ;~(pfix bar (ifix [pal par] (most ace wide))))
          ;~(plug (stag %rock (stag %f (cold | bar))) wede)
          (stag %sand (stag %f (cold | bar)))
        ==
      :-  '~'
        ;~  pose
          rupl
        ::
          ;~  pfix  sig
            ;~  pose
              (stag %clsg (ifix [sel ser] (most ace wide)))
            ::
              %+  stag  %cnsg
              %+  ifix
                [pal par]
              ;~(glam rope wide (most ace wide))
            ::
              (cook (jock |) twid:so)
              (stag [%bust %null] wede)
              (easy [%bust %null])
            ==
          ==
        ==
      :-  '/'
        rood
      :-  '<'
        (ifix [gal gar] (stag %tell (most ace wide)))
      :-  '>'
        (ifix [gar gal] (stag %yell (most ace wide)))
      :-  '#'
        ;~(pfix hax reed)
    ==
  ++  soil
    ;~  pose
      ;~  less  (jest '"""')
        %+  ifix  [doq doq]
        %-  star
        ;~  pose
          ;~(pfix bas ;~(pose bas doq kel bix:ab))
          ;~(less doq bas kel prn)
          (stag ~ sump)
        ==
      ==
    ::
      %-  iny  %+  ifix
        [(jest '"""\0a') (jest '\0a"""')]
      %-  star
      ;~  pose
        ;~(pfix bas ;~(pose bas kel bix:ab))
        ;~(less bas kel prn)
        ;~(less (jest '\0a"""') (just `@`10))
        (stag ~ sump)
      ==
    ==
  ++  sump  (ifix [kel ker] (stag %cltr (most ace wide)))
  ++  norm                                              ::  rune regular form
    |=  tol=?
    |%
    ++  structure
      %-  stew
      ^.  stet  ^.  limo
      :~  :-  '$'
            ;~  pfix  buc
              %-  stew
              ^.  stet  ^.  limo
              :~  [':' (rune col %bccl exqs)]
                  ['%' (rune cen %bccn exqs)]
                  ['<' (rune gal %bcgl exqb)]
                  ['>' (rune gar %bcgr exqb)]
                  ['^' (rune ket %bckt exqb)]
                  ['~' (rune sig %bcsg exqd)]
                  ['|' (rune bar %bcbr exqc)]
                  ['&' (rune pam %bcpm exqc)]
                  ['@' (rune pat %bcpt exqb)]
                  ['_' (rune cab %bccb expa)]
                  ['-' (rune hep %bchp exqb)]
                  ['=' (rune tis %bcts exqg)]
                  ['?' (rune wut %bcwt exqs)]
                  [';' (rune mic %bcmc expa)]
                  ['+' (rune lus %bcls exqg)]
              ==
            ==
        :-  '%'
          ;~  pfix  cen
            %-  stew
            ^.  stet  ^.  limo
            :~  :-  '^'
                %+  cook
                  |=  [%cnkt a=hoon b=spec c=spec d=spec]
                  [%make a b c d ~]
                (rune ket %cnkt exqy)
            ::
                :-  '+'
                %+  cook
                  |=  [%cnls a=hoon b=spec c=spec]
                  [%make a b c ~]
                (rune lus %cnls exqx)
            ::
                :-  '-'
                %+  cook
                  |=  [%cnhp a=hoon b=spec]
                  [%make a b ~]
                (rune hep %cnhp exqd)
            ::
                :-  ':'
                %+  cook
                  |=  [%cncl a=hoon b=(list spec)]
                  [%make a b]
                (rune col %cncl exqz)
            ==
          ==
        :-  '#'
          ;~  pfix  hax  fas
            %+  stag  %bccl
            %+  cook
              |=  [[i=spec t=(list spec)] e=spec]
              [i (snoc t e)]
            ;~  plug
              %+  most  ;~(less ;~(plug fas tar) fas)
              %-  stew
              ^.  stet  ^.  limo
              :~  :-  ['a' 'z']
                  ;~  pose
                    ::  /name=@aura
                    ::
                    %+  cook
                      |=  [=term =aura]
                      ^-  spec
                      :+  %bccl
                        [%leaf %tas aura]
                      :_  ~
                      :+  %bcts  term
                      ?+  aura  [%base %atom aura]
                        %f  [%base %flag]
                        %n  [%base %null]
                      ==
                    ;~(plug sym ;~(pfix tis pat mota))
                  ::
                    ::  /constant
                    ::
                    (stag %leaf (stag %tas ;~(pose sym (cold %$ buc))))
                  ==
                ::
                  ::  /@aura
                  ::
                  :-  '@'
                  %+  cook
                    |=  =aura
                    ^-  spec
                    :+  %bccl
                      [%leaf %tas aura]
                    [%base %atom aura]~
                  ;~(pfix pat mota)
                ::
                  ::  /?
                  ::
                  :-  '?'
                  (cold [%bccl [%leaf %tas %f] [%base %flag] ~] wut)
                ::
                  ::  /~
                  ::
                  :-  '~'
                  (cold [%bccl [%leaf %tas %n] [%base %null] ~] sig)
              ==
            ::
              ::  open-ended or fixed-length
              ::
              ;~  pose
                (cold [%base %noun] ;~(plug fas tar))
                (easy %base %null)
              ==
            ==
          ==
      ==
    ++  expression
      %-  stew
      ^.  stet  ^.  limo
      :~  :-  '|'
            ;~  pfix  bar
              %-  stew
              ^.  stet  ^.  limo
              :~  ['_' (rune cab %brcb exqr)]
                  ['%' (runo cen %brcn ~ expe)]
                  ['@' (runo pat %brpt ~ expe)]
                  [':' (rune col %brcl expb)]
                  ['.' (rune dot %brdt expa)]
                  ['-' (rune hep %brhp expa)]
                  ['^' (rune ket %brkt expr)]
                  ['~' (rune sig %brsg exqc)]
                  ['*' (rune tar %brtr exqc)]
                  ['=' (rune tis %brts exqc)]
                  ['?' (rune wut %brwt expa)]
                  ['$' (rune buc %brbc exqe)]
              ==
            ==
          :-  '$'
            ;~  pfix  buc
              %-  stew
              ^.  stet  ^.  limo
              :~  ['@' (stag %ktcl (rune pat %bcpt exqb))]
                  ['_' (stag %ktcl (rune cab %bccb expa))]
                  [':' (stag %ktcl (rune col %bccl exqs))]
                  ['%' (stag %ktcl (rune cen %bccn exqs))]
                  ['<' (stag %ktcl (rune gal %bcgl exqb))]
                  ['>' (stag %ktcl (rune gar %bcgr exqb))]
                  ['|' (stag %ktcl (rune bar %bcbr exqc))]
                  ['&' (stag %ktcl (rune pam %bcpm exqc))]
                  ['^' (stag %ktcl (rune ket %bckt exqb))]
                  ['~' (stag %ktcl (rune sig %bcsg exqd))]
                  ['-' (stag %ktcl (rune hep %bchp exqb))]
                  ['=' (stag %ktcl (rune tis %bcts exqg))]
                  ['?' (stag %ktcl (rune wut %bcwt exqs))]
                  ['+' (stag %ktcl (rune lus %bcls exqg))]
                  ['.' (rune dot %kttr exqa)]
                  [',' (rune com %ktcl exqa)]
              ==
            ==
          :-  '%'
            ;~  pfix  cen
              %-  stew
              ^.  stet  ^.  limo
              :~  ['_' (rune cab %cncb exph)]
                  ['.' (rune dot %cndt expb)]
                  ['^' (rune ket %cnkt expd)]
                  ['+' (rune lus %cnls expc)]
                  ['-' (rune hep %cnhp expb)]
                  [':' (rune col %cncl expi)]
                  ['~' (rune sig %cnsg expn)]
                  ['*' (rune tar %cntr expm)]
                  ['=' (rune tis %cnts exph)]
              ==
            ==
          :-  ':'
            ;~  pfix  col
              %-  stew
              ^.  stet  ^.  limo
              :~  ['_' (rune cab %clcb expb)]
                  ['^' (rune ket %clkt expd)]
                  ['+' (rune lus %clls expc)]
                  ['-' (rune hep %clhp expb)]
                  ['~' (rune sig %clsg exps)]
                  ['*' (rune tar %cltr exps)]
              ==
            ==
          :-  '.'
            ;~  pfix  dot
              %-  stew
              ^.  stet  ^.  limo
              :~  ['+' (rune lus %dtls expa)]
                  ['*' (rune tar %dttr expb)]
                  ['=' (rune tis %dtts expb)]
                  ['?' (rune wut %dtwt expa)]
                  ['^' (rune ket %dtkt exqn)]
              ==
            ==
          :-  '^'
            ;~  pfix  ket
              %-  stew
              ^.  stet  ^.  limo
              :~  ['|' (rune bar %ktbr expa)]
                  ['.' (rune dot %ktdt expb)]
                  ['-' (rune hep %kthp exqc)]
                  ['+' (rune lus %ktls expb)]
                  ['&' (rune pam %ktpm expa)]
                  ['~' (rune sig %ktsg expa)]
                  ['=' (rune tis %ktts expj)]
                  ['?' (rune wut %ktwt expa)]
                  ['*' (rune tar %kttr exqa)]
                  [':' (rune col %ktcl exqa)]
              ==
            ==
          :-  '~'
            ;~  pfix  sig
              %-  stew
              ^.  stet  ^.  limo
              :~  ['|' (rune bar %sgbr expb)]
                  ['$' (rune buc %sgbc expf)]
                  ['_' (rune cab %sgcb expb)]
                  ['%' (rune cen %sgcn hind)]
                  ['/' (rune fas %sgfs hine)]
                  ['<' (rune gal %sggl hinb)]
                  ['>' (rune gar %sggr hinb)]
                  ['+' (rune lus %sgls hinc)]
                  ['&' (rune pam %sgpm hinf)]
                  ['?' (rune wut %sgwt hing)]
                  ['=' (rune tis %sgts expb)]
                  ['!' (rune zap %sgzp expb)]
              ==
            ==
          :-  ';'
            ;~  pfix  mic
              %-  stew
              ^.  stet  ^.  limo
              :~  [':' (rune col %mccl expi)]
                  ['/' (rune fas %mcfs expa)]
                  ['<' (rune gal %mcgl expz)]
                  ['>' (rune gal %mcgr expzz)]
                  ['~' (rune sig %mcsg expi)]
                  [';' (rune mic %mcmc exqc)]
              ==
            ==
          :-  '='
            ;~  pfix  tis
              %-  stew
              ^.  stet  ^.  limo
              :~  ['|' (rune bar %tsbr exqc)]
                  ['.' (rune dot %tsdt expq)]
                  ['?' (rune wut %tswt expw)]
                  ['^' (rune ket %tskt expt)]
                  [':' (rune col %tscl expp)]
                  ['/' (rune fas %tsfs expo)]
                  [';' (rune mic %tsmc expo)]
                  ['<' (rune gal %tsgl expb)]
                  ['>' (rune gar %tsgr expb)]
                  ['-' (rune hep %tshp expb)]
                  ['*' (rune tar %tstr expg)]
                  [',' (rune com %tscm expb)]
                  ['+' (rune lus %tsls expb)]
                  ['~' (rune sig %tssg expi)]
              ==
            ==
          :-  '?'
            ;~  pfix  wut
              %-  stew
              ^.  stet  ^.  limo
              :~  ['|' (rune bar %wtbr exps)]
                  [':' (rune col %wtcl expc)]
                  ['.' (rune dot %wtdt expc)]
                  ['<' (rune gal %wtgl expb)]
                  ['>' (rune gar %wtgr expb)]
                  ['-' ;~(pfix hep (toad txhp))]
                  ['^' ;~(pfix ket (toad tkkt))]
                  ['=' ;~(pfix tis (toad txts))]
                  ['#' ;~(pfix hax (toad txhx))]
                  ['+' ;~(pfix lus (toad txls))]
                  ['&' (rune pam %wtpm exps)]
                  ['@' ;~(pfix pat (toad tkvt))]
                  ['~' ;~(pfix sig (toad tksg))]
                  ['!' (rune zap %wtzp expa)]
              ==
            ==
          :-  '!'
            ;~  pfix  zap
              %-  stew
              ^.  stet  ^.  limo
              :~  [':' ;~(pfix col (toad expy))]
                  ['.' ;~(pfix dot (toad |.(loaf(bug |))))]
                  [',' (rune com %zpcm expb)]
                  [';' (rune mic %zpmc expb)]
                  ['>' (rune gar %zpgr expa)]
                  ['<' (rune gal %zpgl exqc)]
                  ['@' (rune pat %zppt expx)]
                  ['=' (rune tis %zpts expa)]
                  ['?' (rune wut %zpwt hinh)]
              ==
            ==
      ==
    ::
    ++  boog  !:
      %+  knee  [p=*whit q=*term r=*help s=*hoon]
      |.(~+((scye ;~(pose bola boba))))
    ++  bola                                           ::  ++  arms
      %+  knee  [q=*term r=*help s=*hoon]  |.  ~+
      %+  cook
        |=  [q=term r=whiz s=hoon]
        ?:  =(r *whiz)
          [q *help s]
        [q [[%funk q]~ [r]~] s]
      ;~  pfix  (jest '++')
        ;~  plug
          ;~(pfix gap ;~(pose (cold %$ buc) sym))
          apse:docs
          ;~(pfix jump loaf)
        ==
      ==
    ::TODO consider special casing $%
    ++  boba                                           ::  +$  arms
      %+  knee  [q=*term r=*help s=*hoon]  |.  ~+
      %+  cook
        |=  [q=term r=whiz s=spec]
        ?:  =(r *whiz)
          [q *help [%ktcl %name q s]]
        [q [[%plan q]~ [r]~] [%ktcl %name q s]]
      ;~  pfix  (jest '+$')
        ;~  plug
          ;~(pfix gap sym)
          apse:docs
          ;~(pfix jump loan)
        ==
      ==
   ::
   ::  parses a or [a b c] or a  b  c  ==
   ++  lynx
      =/  wid  (ifix [sel ser] (most ace sym))
      =/  tal
        ;~  sfix
          (most gap sym)
          ;~(plug gap duz)
        ==
      =/  one
        %-  cook  :_  sym
        |=  a=term
        `(list term)`~[a]
      %-  cook
      :_  ;~(pose (runq wid tal) one)
      ::  lestify
      |=  a=(list term)
      ?~(a !! a)
    ::
    ++  whap  !:                                        ::  chapter
      %+  cook
        |=  a=(list (qual whit term help hoon))
        ::  separate $helps into their own list to be passed to +glow
        =/  [duds=(list help) nude=(list (pair term hoon))]
          %+  roll  a
          |=  $:  $=  bog
                  (qual whit term help hoon)
                ::
                  $=  gob
                  [duds=(list help) nude=(list (pair term hoon))]
              ==
          =/  [unt=(list help) tag=(list help)]
            %+  skid  ~(tap by bat.p.bog)  |=(=help =(~ cuff.help))
          :-  ?:  =(*help r.bog)
                (weld tag duds.gob)
              [r.bog (weld tag duds.gob)]
          |-
          ?~  unt  [[q.bog s.bog] nude.gob]
          =.  s.bog  [%note help/i.unt s.bog]
          $(unt t.unt)
        ::
        %+  glow  duds
        |-  ^-  (map term hoon)
        ?~  nude  ~
        =+  $(nude t.nude)
        %+  ~(put by -)
          p.i.nude
        ?:  (~(has by -) p.i.nude)
          [%eror (weld "duplicate arm: +" (trip p.i.nude))]
        q.i.nude
      ::
      (most mush boog)
    ::
    ::  +glow: moves batch comments to the correct arm
    ++  glow
      |=  [duds=(list help) nude=(map term hoon)]
      ^-  (map term hoon)
      |-
      ?~  duds  nude
      ::  if there is no link, its not part of a batch comment
      ?~  cuff.i.duds
        ::  this shouldn't happen yet until we look for cuffs of length >1
        ::  but we need to prove that cuff is nonempty anyways
        $(duds t.duds)
      ::
      ::TODO: look past the first link. this probably requires
      ::a major rethink on how batch comments work
      =/  nom=(unit term)
        ?+    i.cuff.i.duds  ~
        ::  we only support ++ and +$ batch comments right now
        ::
            ?([%funk *] [%plan *])
          `p.i.cuff.i.duds
        ==
      %=  $
        duds  t.duds
        nude  ?~  nom  nude
              ?.  (~(has by nude) u.nom)
                ::  ~>  %slog.[0 leaf+"glow: unmatched link"]
                nude
              (~(jab by nude) u.nom |=(a=hoon [%note help+i.duds a]))
      ==
    ::
    ++  whip                                            ::  chapter declare
      %+  cook
        |=  [[a=whit b=term c=whiz] d=(map term hoon)]
        ^-  [whit (pair term (map term hoon))]
        ?.  =(*whit a)
          [a b d]
        ?:  =(*whiz c)
          [*whit b d]
        [%*(. *whit bat (malt [[%chat b]~ [c]~]~)) b d]
      ;~(plug (seam ;~(pfix (jest '+|') gap cen sym)) whap)
    ::
    ++  wasp                                            ::  $brcb aliases
      ;~  pose
        %+  ifix
          [;~(plug lus tar muck) muck]
        (most muck ;~(gunk sym loll))
      ::
        (easy ~)
      ==
    ::
    ++  wisp  !:                                        ::  core tail
      ?.  tol  fail
      %+  cook
        |=  a=(list [wit=whit wap=(pair term (map term hoon))])
        ^-  (map term tome)
        =<  p
        |-  ^-  (pair (map term tome) (map term hoon))
        ?~  a  [~ ~]
        =/  mor  $(a t.a)
        =.  q.wap.i.a
          %-  ~(urn by q.wap.i.a)
          |=  b=(pair term hoon)  ^+  +.b
          ::  tests for duplicate arms between two chapters
          ?.  (~(has by q.mor) p.b)  +.b
          [%eror (weld "duplicate arm: +" (trip p.b))]
        :_  (~(uni by q.mor) q.wap.i.a)
        %+  ~(put by p.mor)
          p.wap.i.a
        :-  %-  ~(get by bat.wit.i.a)
            ?:  (~(has by bat.wit.i.a) [%chat p.wap.i.a]~)
              [%chat p.wap.i.a]~
            ~
        ?.  (~(has by p.mor) p.wap.i.a)
          q.wap.i.a
        [[%$ [%eror (weld "duplicate chapter: |" (trip p.wap.i.a))]] ~ ~]
      ::
      ::TODO: allow cores with unnamed chapter as well as named chapters?
      ;~  pose
        dun
        ;~  sfix
          ;~  pose
            (most mush whip)
            ;~(plug (stag *whit (stag %$ whap)) (easy ~))
          ==
          gap
          dun
        ==
      ==
    ::
    ::TODO: check parser performance
    ++  toad                                            ::  untrap parser expr
      |*  har=_expa
      =+  dur=(ifix [pal par] $:har(tol |))
      ?.  tol
        dur
      ;~(pose ;~(pfix jump $:har(tol &)) ;~(pfix gap $:har(tol &)) dur)
    ::
    ++  rune                                            ::  build rune
      |*  [dif=rule tuq=* har=_expa]
      ;~(pfix dif (stag tuq (toad har)))
    ::
    ++  runo                                            ::  rune plus
      |*  [dif=rule hil=* tuq=* har=_expa]
      ;~(pfix dif (stag hil (stag tuq (toad har))))
    ::
    ++  runq                                            ::  wide or tall if tol
      |*  [wid=rule tal=rule]                           ::  else wide
      ?.  tol
        wid
      ;~(pose wid tal)
    ::
    ++  butt  |*  zor=rule                              ::  closing == if tall
              ?:(tol ;~(sfix zor ;~(plug gap duz)) zor)
    ++  ulva  |*  zor=rule                              ::  closing -- and tall
              ?.(tol fail ;~(sfix zor ;~(plug gap dun)))
    ++  glop  ~+((glue mash))                           ::  separated by space
    ++  gunk  ~+((glue muck))                           ::  separated list
    ++  goop  ~+((glue mush))                           ::  separator list & docs
    ++  hank  (most mush loaf)                          ::  gapped hoons
    ++  hunk  (most mush loan)                          ::  gapped specs
    ++  jump  ;~(pose leap:docs gap)                    ::  gap before docs
    ++  loaf  ?:(tol tall wide)                         ::  hoon
    ++  loll  ?:(tol tall(doc |) wide(doc |))           ::  hoon without docs
    ++  loan  ?:(tol till wyde)                         ::  spec
    ++  lore  (sear |=(=hoon ~(flay ap hoon)) loaf)     ::  skin
    ++  lomp  ;~(plug sym (punt ;~(pfix tis wyde)))     ::  typeable name
    ++  mash  ?:(tol gap ;~(plug com ace))              ::  list separator
    ++  muss  ?:(tol jump ;~(plug com ace))             ::  list w/ doccords
    ++  muck  ?:(tol gap ace)                           ::  general separator
    ++  mush  ?:(tol jump ace)                          ::  separator w/ docs
    ++  teak  %+  knee  *tiki  |.  ~+                   ::  wing or hoon
              =+  ^=  gub
                  |=  [a=term b=$%([%& p=wing] [%| p=hoon])]
                  ^-  tiki
                  ?-(-.b %& [%& [~ a] p.b], %| [%| [~ a] p.b])
              =+  ^=  wyp
                  ;~  pose
                     %+  cook  gub
                     ;~  plug
                       sym
                       ;~(pfix tis ;~(pose (stag %& rope) (stag %| wide)))
                     ==
                  ::
                     (stag %& (stag ~ rope))
                     (stag %| (stag ~ wide))
                  ==
              ?.  tol  wyp
              ;~  pose
                wyp
              ::
                ;~  pfix
                  ;~(plug ket tis gap)
                  %+  cook  gub
                  ;~  plug
                    sym
                    ;~(pfix gap ;~(pose (stag %& rope) (stag %| tall)))
                  ==
                ==
              ::
                (stag %| (stag ~ tall))
              ==
    ++  rack  (most muss ;~(goop loaf loaf))            ::  list [hoon hoon]
    ++  ruck  (most muss ;~(goop loan loaf))            ::  list [spec hoon]
    ++  rick  (most mash ;~(goop rope loaf))            ::  list [wing hoon]
    ::  hoon contents
    ::
    ++  expa  |.(loaf)                                  ::  one hoon
    ++  expb  |.(;~(goop loaf loaf))                    ::  two hoons
    ++  expc  |.(;~(goop loaf loaf loaf))               ::  three hoons
    ++  expd  |.(;~(goop loaf loaf loaf loaf))          ::  four hoons
    ++  expe  |.(wisp)                                  ::  core tail
    ++  expf  |.(;~(goop ;~(pfix cen sym) loaf))        ::  %term and hoon
    ++  expg  |.(;~(gunk lomp loll loaf))               ::  term/spec, two hoons
    ++  exph  |.((butt ;~(gunk rope rick)))             ::  wing, [wing hoon]s
    ++  expi  |.((butt ;~(goop loaf hank)))             ::  one or more hoons
    ++  expj  |.(;~(goop lore loaf))                    ::  skin and hoon
   :: ++  expk  |.(;~(gunk loaf ;~(plug loaf (easy ~))))::  list of two hoons
   :: ++  expl  |.(;~(gunk sym loaf loaf))              ::  term, two hoons
    ++  expm  |.((butt ;~(gunk rope loaf rick)))        ::  several [spec hoon]s
    ++  expn  |.  ;~  gunk  rope  loaf                  ::  wing, hoon,
                    ;~(plug loaf (easy ~))              ::  list of one hoon
                  ==                                    ::
    ++  expo  |.(;~(goop wise loaf loaf))               ::  =;
    ++  expp  |.(;~(goop (butt rick) loaf))             ::  [wing hoon]s, hoon
    ++  expq  |.(;~(goop rope loaf loaf))               ::  wing and two hoons
    ++  expr  |.(;~(goop loaf wisp))                    ::  hoon and core tail
    ++  exps  |.((butt hank))                           ::  closed gapped hoons
    ++  expt  |.(;~(gunk wise rope loaf loaf))          ::  =^
    ++  expu  |.(;~(gunk rope loaf (butt hank)))        ::  wing, hoon, hoons
   :: ++  expv  |.((butt rick))                         ::  just changes
    ++  expw  |.(;~(goop rope loaf loaf loaf))          ::  wing and three hoons
    ++  expx  |.(;~(goop ropa loaf loaf))               ::  wings and two hoons
    ++  expy  |.(loaf(bug &))                           ::  hoon with tracing
    ++  expz  |.(;~(goop loan loaf loaf loaf))          ::  spec and three hoons
    ++  expzz  |.(;~(goop loan loaf loaf))              ::  spec and two hoons
    ::  spec contents
    ::
    ++  exqa  |.(loan)                                  ::  one spec
    ++  exqb  |.(;~(goop loan loan))                    ::  two specs
    ++  exqc  |.(;~(goop loan loaf))                    ::  spec then hoon
    ++  exqd  |.(;~(goop loaf loan))                    ::  hoon then spec
    ++  exqe  |.(;~(goop lynx loan))                    ::  list of names then spec
    ++  exqs  |.((butt hunk))                           ::  closed gapped specs
    ++  exqg  |.(;~(goop sym loan))                     ::  term and spec
    ::++  exqk  |.(;~(goop loaf ;~(plug loan (easy ~))))::  hoon with one spec
    ++  exqn  |.(;~(gunk loan (stag %cltr (butt hank))))::  autoconsed hoons
    ++  exqr  |.(;~(gunk loan ;~(plug wasp wisp)))      ::  spec/aliases?/tail
    ::++  exqw  |.(;~(goop loaf loan))                  ::  hoon and spec
    ++  exqx  |.(;~(goop loaf loan loan))               ::  hoon, two specs
    ++  exqy  |.(;~(goop loaf loan loan loan))          ::  hoon, three specs
    ++  exqz  |.(;~(goop loaf (butt hunk)))             ::  hoon, n specs
    ::
    ::    tiki expansion for %wt runes
    ::
    ++  txhp  |.  %+  cook  |=  [a=tiki b=(list (pair spec hoon))]
                            (~(wthp ah a) b)
                  (butt ;~(gunk teak ruck))
    ++  tkkt  |.  %+  cook  |=  [a=tiki b=hoon c=hoon]
                            (~(wtkt ah a) b c)
                  ;~(gunk teak loaf loaf)
    ++  txls  |.  %+  cook  |=  [a=tiki b=hoon c=(list (pair spec hoon))]
                            (~(wtls ah a) b c)
                  (butt ;~(gunk teak loaf ruck))
    ++  tkvt  |.  %+  cook  |=  [a=tiki b=hoon c=hoon]
                            (~(wtpt ah a) b c)
                  ;~(gunk teak loaf loaf)
    ++  tksg  |.  %+  cook  |=  [a=tiki b=hoon c=hoon]
                            (~(wtsg ah a) b c)
                  ;~(gunk teak loaf loaf)
    ++  txts  |.  %+  cook  |=  [a=spec b=tiki]
                            (~(wtts ah b) a)
                  ;~(gunk loan teak)
    ++  txhx  |.  %+  cook  |=  [a=skin b=tiki]
                            (~(wthx ah b) a)
                  ;~(gunk lore teak)
    ::
    ::  hint syntax
    ::
    ++  hinb  |.(;~(goop bont loaf))                    ::  hint and hoon
    ++  hinc  |.                                        ::  optional =en, hoon
              ;~(pose ;~(goop bony loaf) (stag ~ loaf)) ::
    ++  hind  |.(;~(gunk bonk loaf ;~(goop bonz loaf))) ::  jet hoon "bon"s hoon
    ++  hine  |.(;~(goop bonk loaf))                    ::  jet-hint and hoon
    ++  hinf  |.                                        ::  0-3 >s, two hoons
      ;~  pose
        ;~(goop (cook lent (stun [1 3] gar)) loaf loaf)
        (stag 0 ;~(goop loaf loaf))
      ==
    ++  hing  |.                                        ::  0-3 >s, three hoons
      ;~  pose
        ;~(goop (cook lent (stun [1 3] gar)) loaf loaf loaf)
        (stag 0 ;~(goop loaf loaf loaf))
      ==
    ++  bonk                                            ::  jet signature
      ;~  pfix  cen
        ;~  pose
          ;~(plug sym ;~(pfix col ;~(plug sym ;~(pfix dot ;~(pfix dot dem)))))
          ;~(plug sym ;~(pfix col ;~(plug sym ;~(pfix dot dem))))
          ;~(plug sym ;~(pfix dot dem))
          sym
        ==
      ==
    ++  hinh  |.                                        ::  1/2 numbers, hoon
        ;~  goop
          ;~  pose
            dem
            (ifix [sel ser] ;~(plug dem ;~(pfix ace dem)))
          ==
          loaf
        ==
    ++  bont  ;~  (bend)                                ::  term, optional hoon
                ;~(pfix cen sym)
                ;~(pfix dot ;~(pose wide ;~(pfix muck loaf)))
              ==
    ++  bony  (cook |=(a=(list) (lent a)) (plus tis))   ::  base 1 =en count
    ++  bonz                                            ::  term-labelled hoons
      ;~  pose
        (cold ~ sig)
        %+  ifix
          ?:(tol [;~(plug duz gap) ;~(plug gap duz)] [pal par])
        (more mash ;~(gunk ;~(pfix cen sym) loaf))
      ==
    --
  ::
  ++  lang                                              ::  lung sample
    $:  ros=hoon
        $=  vil
        $%  [%tis p=hoon]
            [%col p=hoon]
            [%ket p=hoon]
            [%lit p=(list (pair wing hoon))]
        ==
    ==
  ::
  ++  lung
    ~+
    %-  bend
    |:  $:lang
    ^-  (unit hoon)
    ?-    -.vil
      %col  ?:(=([%base %flag] ros) ~ [~ %tsgl ros p.vil])
      %lit  (bind ~(reek ap ros) |=(hyp=wing [%cnts hyp p.vil]))
      %ket  [~ ros p.vil]
      %tis  =+  rud=~(flay ap ros)
            ?~(rud ~ `[%ktts u.rud p.vil])
    ==
  ::
  ++  long
    %+  knee  *hoon  |.  ~+
    ;~  lung
      scat
      ;~  pose
        ;~(plug (cold %tis tis) wide)
        ;~(plug (cold %col col) wide)
        ;~(plug (cold %ket ket) wide)
        ;~  plug
          (easy %lit)
          (ifix [pal par] lobo)
        ==
      ==
    ==
  ::
  ++  lobo  (most ;~(plug com ace) ;~(glam rope wide))
  ++  loon  (most ;~(plug com ace) ;~(glam wide wide))
  ++  lute                                              ::  tall [] noun
    ~+
    %+  cook  |=(hoon +<)
    %+  stag  %cltr
    %+  ifix
      [;~(plug sel gap) ;~(plug gap ser)]
    (most gap tall)
  ::
  ++  ropa  (most col rope)
  ++  rope                                              ::  wing form
    %+  knee  *wing
    |.  ~+
    %+  (slug |=([a=limb b=wing] [a b]))
      dot
    ;~  pose
      (cold [%| 0 ~] com)
      %+  cook
        |=([a=(list) b=term] ?~(a b [%| (lent a) `b]))
      ;~(plug (star ket) ;~(pose sym (cold %$ buc)))
    ::
      %+  cook
        |=(a=axis [%& a])
      ;~  pose
        ;~(pfix lus dim:ag)
        ;~(pfix pam (cook |=(a=@ ?:(=(0 a) 0 (mul 2 +($(a (dec a)))))) dim:ag))
        ;~(pfix bar (cook |=(a=@ ?:(=(0 a) 1 +((mul 2 $(a (dec a)))))) dim:ag))
        ven
        (cold 1 dot)
      ==
    ==
  ::
  ++  wise
    ;~  pose
      ;~  pfix  tis
        %+  sear
          |=  =spec
          ^-  (unit skin)
          %+  bind  ~(autoname ax spec)
          |=  =term
          [%name term %spec spec %base %noun]
        wyde
      ==
    ::
      %+  cook
        |=  [=term =(unit spec)]
        ^-  skin
        ?~  unit
          term
        [%name term %spec u.unit %base %noun]
      ;~  plug  sym
        (punt ;~(pfix ;~(pose fas tis) wyde))
      ==
    ::
      %+  cook
        |=  =spec
        ^-  skin
        [%spec spec %base %noun]
      wyde
    ==
  ::
  ++  tall                                              ::  full tall form
    %+  knee  *hoon
    |.(~+((wart (clad ;~(pose expression:(norm &) long lute apex:(sail &))))))
  ++  till                                              ::  mold tall form
    %+  knee  *spec
    |.(~+((wert (coat ;~(pose structure:(norm &) scad)))))
  ++  wede                                              ::  wide bulb
    ::  XX: lus deprecated
    ::
    ;~(pfix ;~(pose lus fas) wide)
  ++  wide                                              ::  full wide form
    %+  knee  *hoon
    |.(~+((wart ;~(pose expression:(norm |) long apex:(sail |)))))
  ++  wyde                                              ::  mold wide form
    %+  knee  *spec
    |.(~+((wert ;~(pose structure:(norm |) scad))))
  ++  wart
    |*  zor=rule
    %+  here
      |=  [a=pint b=hoon]
      ?:(bug [%dbug [wer a] b] b)
    zor
  ++  wert
    |*  zor=rule
    %+  here
      |=  [a=pint b=spec]
      ?:(bug [%dbug [wer a] b] b)
    zor
  --
--
