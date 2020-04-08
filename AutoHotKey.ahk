;======================== Start Auto-Execution Section ==============================

; Keeps script permanently running
#Persistent

; Avoids checking empty variables to see if they are environment variables
; Recommended for performance and compatibility with future AutoHotkey releases
#NoEnv

; Ensures that there is only a single instance of this script running
#SingleInstance, Force

;Determines whether invisible windows are "seen" by the script
DetectHiddenWindows, On

; Makes a script unconditionally use its own folder as its working directory
; Ensures a consistent starting directory
SetWorkingDir %A_ScriptDir%

; sets title matching to search for "containing" isntead of "exact"
SetTitleMatchMode, 2

; Recommended for new scripts due to its superior speed and reliability
SendMode Input

;; deactivate capslock completely
SetCapslockState, AlwaysOff

; # = Win
; ! = Alt
; ^ = Ctrl
; + = Shift

; EditWithVSCode()
; {
;     Run "C:\w10dev\Microsoft VS Code\Code.exe" "%A_ScriptFullPath%"
; }
; 
; ; Remove the standard menu items temporarily
; Menu, Tray, NoStandard 
; ; Add our custom menu item labeled "Edit With VS Code" 
; ; and calls the function above
; Menu, Tray, Add, Edit With VS Code, EditWithVSCode
; ; Add a separator
; Menu, Tray, Add 
; ; Put the standard menu items back, under our custom menu item
; Menu, Tray, Standard 

;
; RegWrite, REG_SZ, HKEY_CURRENT_USER, Software\Classes\ahk_auto_file\shell\edit\command,, "%UserInput%" "`%1"
; 
; Or, with PowerShell
; PS HKCU:\Software\Classes\ahk_auto_file\shell\edit> New-ItemProperty -Name "(default)" -Value '"C:\w10dev\Microsoft VS Code\Code.exe" "%1"' -PropertyType String -Path command

;======================== Start Auto-Execution Section ==============================



; Menu, Folders, Add, &AutoHotkey,!1
; Menu, Folders, Add, &Dropbox,!2
; Menu, Folders, Add, &w10dev,!3
; !1:: Run, C:\Users\u68320\AutoHotkey
; !2:: Run, C:\Users\u68320\Dropbox
; !3:: Run, C:\w10dev
; 
; !x::Menu, Folders, Show

;#IfWinActive emacs  ; if in emacs
;#IfWinActive        ; end if in emacs


;----------------------------
; Turn Capslock into Control
;----------------------------
+Capslock::Capslock ; make shift+Caps-Lock the Caps Lock toggle
Capslock::Control   ; make Caps Lock the control button


; Look at creating a 'Hyper' key that responds differently for long and short presses
; AHK TapHoldManager looks good: https://www.autohotkey.com/boards/viewtopic.php?t=45116
; https://github.com/evilC/TapHoldManager


;------------------------------
; Spotify Control
; Thanks https://www.reddit.com/r/AutoHotkey/comments/4lr9n0/chrome_unresponsive_to_controlsend/
;#IfWinNotActive, ahk_exe chrome.exe


; Redirect Volume and Play/Pause buttons away from TurboVNC
#IfWinActive, ahk_class VNCViewer
Media_Play_Pause::ControlSend ahk_parent, {Media_Play_Pause}, ahk_class Shell_TrayWnd
Volume_Up::ControlSend ahk_parent, {Volume_Up}, ahk_exe chrome.exe
Volume_Down::ControlSend ahk_parent, {Volume_Down}, ahk_class Shell_TrayWnd
Volume_Mute::
{
   SplashTextOn,,,Mute Pressed,
   Sleep,500
   SplashTextOff
}
return
#IfWinActive

; For use on work mini keyboard only
;Browser_Home::Media_Play_Pause
;Browser_Favorites::Volume_Up
;Launch_Mail::Volume_Down

Launch_App2::
	toggle:=!toggle ; This toggles the variable between true/false
	if toggle
	{
		Run "C:\w10dev\nircmd-x64\nircmd.exe" setdefaultsounddevice "Speakers"
		soundToggleBox("Speakers")
	}
	else
	{
		Run "C:\w10dev\nircmd-x64\nircmd.exe" setdefaultsounddevice "Jabra Evolve"
		soundToggleBox("Jabra Evolve")
	}
Return

; Display sound toggle GUI
soundToggleBox(Device)
{
	IfWinExist, soundToggleWin
	{
		Gui, destroy
	}
	
	Gui, +ToolWindow -Caption +0x400000 +alwaysontop
	Gui, Add, text, x35 y8, Default sound: %Device%
	SysGet, screenx, 0
	SysGet, screeny, 1
	xpos:=screenx-275
	ypos:=screeny-100
	Gui, Show, NoActivate x%xpos% y%ypos% h30 w200, soundToggleWin
	
	SetTimer,soundToggleClose, 2000
}
soundToggleClose:
    SetTimer,soundToggleClose, off
    Gui, destroy
Return


;	; Gets the control ID of google chrome
;	ControlGet, controlID, Hwnd,,Chrome_RenderWidgetHostHWND1, Google Chrome
;
;	; Focuses on chrome without breaking focus on what you're doing
;	ControlFocus,,ahk_id %controlID%
;
;   Activated := false
;   if not WinActive("Google Chrome ahk_exe chrome.exe") {
;      Activated := true
;      WinActivate, Google Chrome ahk_exe chrome.exe
;   }
;	
;	ControlSend, Chrome_RenderWidgetHostHWND1, {Space}, Google Chrome
;
;   if (Activated = true) {
;      ; Switch back to previous app
;      Send, {ALT DOWN}{TAB}{ALT UP}
;      ; Send, !{Esc}
;      ; WinMinimize, Google Chrome ahk_exe chrome.exe
;   }
;
;return

;#IfWinNotActive
; Browser_Home::
; 	ControlSend, ahk_parent, {Space}, ahk_exe Chrome.exe 
; return
;Browser_Favorites::Send {Volume_Up}
;Launch_Mail::Send {Volume_Down}


;-------------------------------------------------
; Copy URL with Title
^!Space::
WinGetTitle, Title, A ; grab the title of the active window
clipboard := ""   ; Empty the clipboard.
SendInput ^l ; Select URL
Sleep, 200
SendInput ^a^c ; copy the url
ClipWait
Title := StrReplace(Title, " - Mozilla Firefox")
clipboard := "[" Title "](" clipboard ")"
; MsgBox, The active window is "%Title%".
; FormatTime, now,, yyyy/MM/dd HH:mm
; SendInput %now%
return

^!k::WinMaximize, A  ; Maximise active window on Ctrl+Alt+k


; Insert markdown link
:*:;mdl::
MoveBack := StrLen(Clipboard) + 3
Send [](%Clipboard%){Left %MoveBack%}
return



::;dt::
FormatTime, now,, yyyy/MM/dd HH:mm
SendInput %now%
return


:*:;wemail::damien.ayers@ga.gov.au
:*:;email::damien@omad.net
::;sh::/g/data/u46/users/dra547/
:*:;shrug::¯\_(ツ)_/¯
:*:;lenny::( ͡° ͜ʖ ͡°)

:*:h.odc::https://github.com/opendatacube/
:*:h.ga::https://github.com/GeoscienceAustralia/



:*:;dailystandup::
PrevDay := PrevBusinessDay()
FormatTime, fPrev, %PrevDay%, dddd d
FormatTime, fToday,, dddd d
ClipSaved := ClipboardAll
Clipboard = 
( RTrim0
*Yesterday (%fPrev%)*
- 
- 
*Today (%fToday%)*
- 
- 
)
Send ^v
Send {up 5}
;Clipboard := ClipSaved
;ClipSaved := ""
return

:*:;gaw::git@github.com:GeoscienceAustralia/wofs.git
:*:;gafc::git@github.com:GeoscienceAustralia/fc.git
:*:;gaeod::git@github.com:GeoscienceAustralia/eodatasets.git


PrevBusinessDay() {
   prevday := A_Now
   if (A_DDD = "Mon") {
      prevday += -3, Days
   } else {
      prevday += -1, Days
   }
   return prevday
}


:*:]d::  ; This hotstring replaces "]d" with the current date and time via the commands below.
FormatTime, CurrentDateTime,, M/d/yyyy h:mm tt  ; It will look like 9/1/2005 3:53 PM
SendInput %CurrentDateTime%
return


DllCall( "AddFontResource", Str,"c:\Users\u68320\Downloads\Inconsolata-Regular.ttf" )
DllCall( "AddFontResource", Str,"c:\Users\u68320\Downloads\Inconsolata-Bold.ttf" )
DllCall( "AddFontResource", Str,"c:\Users\u68320\Downloads\Meslo\MesloLGMDZ-Bold.ttf" )
DllCall( "AddFontResource", Str,"c:\Users\u68320\Downloads\Meslo\MesloLGMDZ-BoldItalic.ttf" )
DllCall( "AddFontResource", Str,"c:\Users\u68320\Downloads\Meslo\MesloLGMDZ-Regular.ttf" )
DllCall( "AddFontResource", Str,"c:\Users\u68320\Downloads\Meslo\MesloLGMDZ-Italic.ttf" )
SendMessage,  0x1D,,,, ahk_id 0xFFFF


;# FAKE More buttons on my mouse
; ARGH, Prevents ALT-RightClick working in Linux VMs
;RButton::click right
;RButton & WheelDown::Send {Browser_Back}
;RButton & WheelUp::Send {Browser_Forward}



;-------------------------------------------------------------------------------
; Auto-Reload AutoHotkey when .ahk file is saved
; http://prxbx.com/forums/showthread.php?tid=1181
; Modified 2009-12-09 11:32:17 by Luke Scammell - luke {at} scammell [dot] co (.) uk
; Modified to match any window with .ahk in the title, meaning it will update other scripts as well and from other programs like Notepad++ :)
~^s::
SetTitleMatchMode, 2
IfWinActive, .ahk
{
   Send, ^s
   SplashTextOn,,,Updated script,
   Sleep,500
   SplashTextOff
   Reload
}
else
   Send, ^s
return
;------------------------------------------------------------------------------/