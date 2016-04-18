
ttk::style theme use clam

##################################################
## General

ttk::style configure "." \
    -background       grey95 \
    -bordercolor      grey50 \
    -borderwidth      1 \
    -cursor           arrow \
    -darkcolor        grey95 \
    -fieldbackground  white \
    -foreground       black \
    -lightcolor       grey95 \
    -relief           flat \
    -selectbackground "#0064FF" \
    -selectforeground white \
    -insertwidth      1 \
;

##################################################
## TButton

ttk::style configure "TButton" \
    -background grey85 \
    -darkcolor  grey85 \
    -lightcolor grey85 \
;
ttk::style map "TButton" \
    -background  {disabled grey95 active aliceblue} \
    -bordercolor {disabled grey75 active "#0064FF"} \
    -darkcolor   {disabled grey95 active aliceblue} \
    -lightcolor  {disabled grey95 active aliceblue} \
    -foreground  {disabled grey75}
;

##################################################
## TCheckbutton

ttk::style configure "TCheckbutton" \
    -padding {5 0} \
;
ttk::style map "TCheckbutton" \
    -background {disabled grey95} \
    -indicatorbackground {disabled grey95} \
    -indicatorforeground {disabled grey75} \
    -lowerbordercolor    {disabled grey75} \
    -upperbordercolor    {disabled grey75} \
;

##################################################
## TCombobox

ttk::style map "TCombobox" \
    -background      {disabled grey95} \
    -bordercolor     {disabled grey75 focus "#0064FF"} \
    -darkcolor       {disabled grey95 focus white} \
    -lightcolor      {disabled grey95 focus white} \
    -fieldbackground {disabled grey95} \
    -fieldforeground {disabled grey75} \
    -foreground      {disabled grey75} \
;

##################################################
## TEntry

ttk::style map "TEntry" \
    -background      {disabled grey95} \
    -bordercolor     {disabled grey75 focus "#0064FF"} \
    -darkcolor       {disabled grey95 focus white} \
    -lightcolor      {disabled grey95 focus white} \
    -fieldbackground {disabled grey95} \
    -fieldforeground {disabled grey75} \
    -foreground      {disabled grey75} \
;

##################################################
## TLabel

ttk::style configure "TLabel" \
    -padding {0 0} \
;
ttk::style map "TLabel" \
    -background {disabled grey95} \
    -foreground {disabled black} \
;

##################################################
## TNotebook.Tab

ttk::style configure "TNotebook.Tab" \
    -background grey85 \
    -darkcolor  grey85 \
    -lightcolor grey85 \
;
ttk::style map "TNotebook.Tab" \
    -background {active aliceblue selected grey95} \
    -darkcolor  {active aliceblue selected grey95} \
    -lightcolor {active aliceblue selected grey95} \
;

##################################################
## TRadiobutton

ttk::style map "TRadiobutton" \
    -background {disabled grey95} \
    -indicatorbackground {disabled grey95} \
    -indicatorforeground {disabled grey75} \
    -lowerbordercolor    {disabled grey75} \
    -upperbordercolor    {disabled grey75} \
;

##################################################
## TScrollbar

ttk::style configure "TScrollbar" \
    -arrowcolor  grey50 \
    -arrowsize   15 \
    -background  grey75 \
    -troughcolor grey95 \
;
ttk::style map "TScrollbar" \
    -background { active aliceblue } \
;




