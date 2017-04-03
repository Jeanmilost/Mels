// *************************************************************************************************
// * ==> UTQRLogging ------------------------------------------------------------------------------*
// *************************************************************************************************
// * MIT License - The Mels Library, a free and easy-to-use 3D Models library                      *
// *                                                                                               *
// * Permission is hereby granted, free of charge, to any person obtaining a copy of this software *
// * and associated documentation files (the "Software"), to deal in the Software without          *
// * restriction, including without limitation the rights to use, copy, modify, merge, publish,    *
// * distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the *
// * Software is furnished to do so, subject to the following conditions:                          *
// *                                                                                               *
// * The above copyright notice and this permission notice shall be included in all copies or      *
// * substantial portions of the Software.                                                         *
// *                                                                                               *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING *
// * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND    *
// * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  *
// * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      *
// * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *
// *************************************************************************************************

{**
 @abstract(@name provides the features to log runtime messages to output like e.g. compiler console.)
 @image(Resources/Images/Documentation/Mels.svg)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit UTQRLogging;

{$MODE Delphi}

interface

uses SysUtils,
     Classes,
     Controls,
     Windows;

type
    {$REGION 'Documentation'}
    {**
     Helper class for message logging
    }
    {$ENDREGION}
    TQRLogHelper = class
        private
            class var m_LastTime: NativeUInt;

        public
            {$REGION 'Documentation'}
            {**
             Constructor
            }
            {$ENDREGION}
            constructor Create; virtual;

            {$REGION 'Documentation'}
            {**
             Destructor
            }
            {$ENDREGION}
            destructor Destroy; override;

            {$REGION 'Documentation'}
            {**
             Logs a message to the compiler console
             @param(msg Message to log)
            }
            {$ENDREGION}
            class procedure LogToCompiler(const msg: UnicodeString); static;

            {$REGION 'Documentation'}
            {**
             Begins an elapsed time measurement
            }
            {$ENDREGION}
            class procedure BeginElapsedTimeLog; static;

            {$REGION 'Documentation'}
            {**
             Logs an elapsed time value to the compiler console
             @param(name Name that will describe the measurement)
            }
            {$ENDREGION}
            class procedure LogElapsedTimeToCompiler(const name: UnicodeString); static;

            {$REGION 'Documentation'}
            {**
             Convert Windows message as string
             @param(pOwner Component that owns the message)
             @param(message Windows message to convert)
             @return(Windows message as string)
            }
            {$ENDREGION}
            class function WinMsgToStr(pOwner: TComponent;
                                const message: TMessage): UnicodeString; static;
    end;

implementation
//--------------------------------------------------------------------------------------------------
// TQRLogHelper
//--------------------------------------------------------------------------------------------------
constructor TQRLogHelper.Create;
begin
    inherited Create;
end;
//--------------------------------------------------------------------------------------------------
destructor TQRLogHelper.Destroy;
begin
    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
class procedure TQRLogHelper.LogToCompiler(const msg: UnicodeString);
begin
    OutputDebugString(PChar(msg));
end;
//--------------------------------------------------------------------------------------------------
class procedure TQRLogHelper.BeginElapsedTimeLog;
begin
    m_LastTime := GetTickCount;
end;
//--------------------------------------------------------------------------------------------------
class procedure TQRLogHelper.LogElapsedTimeToCompiler(const name: UnicodeString);
var
    now: NativeUInt;
begin
    // get current time
    now := GetTickCount;

    // log current time in milliseconds
    OutputDebugString(PChar(AnsiString(name) + ' - ' + IntToStr(now - m_LastTime) + 'ms'));

    // update last time
    m_LastTime := now;
end;
//--------------------------------------------------------------------------------------------------
class function TQRLogHelper.WinMsgToStr(pOwner: TComponent; const message: TMessage): UnicodeString;
var
    name, wParamName, lParamName: UnicodeString;
begin
    // get messag name
    case (message.Msg) of
        // standard Windows messages. This list was created using the Wine documentation:
        // https://wiki.winehq.org/List_Of_Windows_Messages
        WM_NULL:                        name := 'WM_NULL';
        WM_CREATE:                      name := 'WM_CREATE';
        WM_DESTROY:                     name := 'WM_DESTROY';
        WM_MOVE:                        name := 'WM_MOVE';
        WM_SIZE:                        name := 'WM_SIZE';
        WM_ACTIVATE:                    name := 'WM_ACTIVATE';
        WM_SETFOCUS:                    name := 'WM_SETFOCUS';
        WM_KILLFOCUS:                   name := 'WM_KILLFOCUS';
        WM_ENABLE:                      name := 'WM_ENABLE';
        WM_SETREDRAW:                   name := 'WM_SETREDRAW';
        WM_SETTEXT:                     name := 'WM_SETTEXT';
        WM_GETTEXT:                     name := 'WM_GETTEXT';
        WM_GETTEXTLENGTH:               name := 'WM_GETTEXTLENGTH';
        WM_PAINT:                       name := 'WM_PAINT';
        WM_CLOSE:                       name := 'WM_CLOSE';
        WM_QUERYENDSESSION:             name := 'WM_QUERYENDSESSION';
        WM_QUIT:                        name := 'WM_QUIT';
        WM_QUERYOPEN:                   name := 'WM_QUERYOPEN';
        WM_ERASEBKGND:                  name := 'WM_ERASEBKGND';
        WM_SYSCOLORCHANGE:              name := 'WM_SYSCOLORCHANGE';
        WM_ENDSESSION:                  name := 'WM_ENDSESSION';
        WM_SHOWWINDOW:                  name := 'WM_SHOWWINDOW';
        WM_CTLCOLOR:                    name := 'WM_CTLCOLOR';
        WM_WININICHANGE:                name := 'WM_WININICHANGE';
        WM_DEVMODECHANGE:               name := 'WM_DEVMODECHANGE';
        WM_ACTIVATEAPP:                 name := 'WM_ACTIVATEAPP';
        WM_FONTCHANGE:                  name := 'WM_FONTCHANGE';
        WM_TIMECHANGE:                  name := 'WM_TIMECHANGE';
        WM_CANCELMODE:                  name := 'WM_CANCELMODE';
        WM_SETCURSOR:                   name := 'WM_SETCURSOR';
        WM_MOUSEACTIVATE:               name := 'WM_MOUSEACTIVATE';
        WM_CHILDACTIVATE:               name := 'WM_CHILDACTIVATE';
        WM_QUEUESYNC:                   name := 'WM_QUEUESYNC';
        WM_GETMINMAXINFO:               name := 'WM_GETMINMAXINFO';
        WM_PAINTICON:                   name := 'WM_PAINTICON';
        WM_ICONERASEBKGND:              name := 'WM_ICONERASEBKGND';
        WM_NEXTDLGCTL:                  name := 'WM_NEXTDLGCTL';
        WM_SPOOLERSTATUS:               name := 'WM_SPOOLERSTATUS';
        WM_DRAWITEM:                    name := 'WM_DRAWITEM';
        WM_MEASUREITEM:                 name := 'WM_MEASUREITEM';
        WM_DELETEITEM:                  name := 'WM_DELETEITEM';
        WM_VKEYTOITEM:                  name := 'WM_VKEYTOITEM';
        WM_CHARTOITEM:                  name := 'WM_CHARTOITEM';
        WM_SETFONT:                     name := 'WM_SETFONT';
        WM_GETFONT:                     name := 'WM_GETFONT';
        WM_SETHOTKEY:                   name := 'WM_SETHOTKEY';
        WM_GETHOTKEY:                   name := 'WM_GETHOTKEY';
        WM_QUERYDRAGICON:               name := 'WM_QUERYDRAGICON';
        WM_COMPAREITEM:                 name := 'WM_COMPAREITEM';
        WM_GETOBJECT:                   name := 'WM_GETOBJECT';
        WM_COMPACTING:                  name := 'WM_COMPACTING';
        $44:                            name := 'WM_COMMNOTIFY';
        WM_WINDOWPOSCHANGING:           name := 'WM_WINDOWPOSCHANGING';
        WM_WINDOWPOSCHANGED:            name := 'WM_WINDOWPOSCHANGED';
        WM_POWER:                       name := 'WM_POWER';
        $49:                            name := 'WM_COPYGLOBALDATA';
        WM_COPYDATA:                    name := 'WM_COPYDATA';
        WM_CANCELJOURNAL:               name := 'WM_CANCELJOURNAL';
        WM_NOTIFY:                      name := 'WM_NOTIFY';
        WM_INPUTLANGCHANGEREQUEST:      name := 'WM_INPUTLANGCHANGEREQUEST';
        WM_INPUTLANGCHANGE:             name := 'WM_INPUTLANGCHANGE';
        WM_TCARD:                       name := 'WM_TCARD';
        WM_HELP:                        name := 'WM_HELP';
        WM_USERCHANGED:                 name := 'WM_USERCHANGED';
        WM_NOTIFYFORMAT:                name := 'WM_NOTIFYFORMAT';
        WM_CONTEXTMENU:                 name := 'WM_CONTEXTMENU';
        WM_STYLECHANGING:               name := 'WM_STYLECHANGING';
        WM_STYLECHANGED:                name := 'WM_STYLECHANGED';
        WM_DISPLAYCHANGE:               name := 'WM_DISPLAYCHANGE';
        WM_GETICON:                     name := 'WM_GETICON';
        WM_SETICON:                     name := 'WM_SETICON';
        WM_NCCREATE:                    name := 'WM_NCCREATE';
        WM_NCDESTROY:                   name := 'WM_NCDESTROY';
        WM_NCCALCSIZE:                  name := 'WM_NCCALCSIZE';
        WM_NCHITTEST:                   name := 'WM_NCHITTEST';
        WM_NCPAINT:                     name := 'WM_NCPAINT';
        WM_NCACTIVATE:                  name := 'WM_NCACTIVATE';
        WM_GETDLGCODE:                  name := 'WM_GETDLGCODE';
        $88:                            name := 'WM_SYNCPAINT';
        WM_NCMOUSEMOVE:                 name := 'WM_NCMOUSEMOVE';
        WM_NCLBUTTONDOWN:               name := 'WM_NCLBUTTONDOWN';
        WM_NCLBUTTONUP:                 name := 'WM_NCLBUTTONUP';
        WM_NCLBUTTONDBLCLK:             name := 'WM_NCLBUTTONDBLCLK';
        WM_NCRBUTTONDOWN:               name := 'WM_NCRBUTTONDOWN';
        WM_NCRBUTTONUP:                 name := 'WM_NCRBUTTONUP';
        WM_NCRBUTTONDBLCLK:             name := 'WM_NCRBUTTONDBLCLK';
        WM_NCMBUTTONDOWN:               name := 'WM_NCMBUTTONDOWN';
        WM_NCMBUTTONUP:                 name := 'WM_NCMBUTTONUP';
        WM_NCMBUTTONDBLCLK:             name := 'WM_NCMBUTTONDBLCLK';
        WM_NCXBUTTONDOWN:               name := 'WM_NCXBUTTONDOWN';
        WM_NCXBUTTONUP:                 name := 'WM_NCXBUTTONUP';
        WM_NCXBUTTONDBLCLK:             name := 'WM_NCXBUTTONDBLCLK';
        EM_GETSEL:                      name := 'EM_GETSEL';
        EM_SETSEL:                      name := 'EM_SETSEL';
        EM_GETRECT:                     name := 'EM_GETRECT';
        EM_SETRECT:                     name := 'EM_SETRECT';
        EM_SETRECTNP:                   name := 'EM_SETRECTNP';
        EM_SCROLL:                      name := 'EM_SCROLL';
        EM_LINESCROLL:                  name := 'EM_LINESCROLL';
        EM_SCROLLCARET:                 name := 'EM_SCROLLCARET';
        EM_GETMODIFY:                   name := 'EM_GETMODIFY';
        EM_SETMODIFY:                   name := 'EM_SETMODIFY';
        EM_GETLINECOUNT:                name := 'EM_GETLINECOUNT';
        EM_LINEINDEX:                   name := 'EM_LINEINDEX';
        EM_SETHANDLE:                   name := 'EM_SETHANDLE';
        EM_GETHANDLE:                   name := 'EM_GETHANDLE';
        EM_GETTHUMB:                    name := 'EM_GETTHUMB';
        EM_LINELENGTH:                  name := 'EM_LINELENGTH';
        EM_REPLACESEL:                  name := 'EM_REPLACESEL';
        $c3:                            name := 'EM_SETFONT';
        EM_GETLINE:                     name := 'EM_GETLINE';
        EM_SETLIMITTEXT:                name := 'EM_SETLIMITTEXT';
        EM_CANUNDO:                     name := 'EM_CANUNDO';
        EM_UNDO:                        name := 'EM_UNDO';
        EM_FMTLINES:                    name := 'EM_FMTLINES';
        EM_LINEFROMCHAR:                name := 'EM_LINEFROMCHAR';
        $ca:                            name := 'EM_SETWORDBREAK';
        EM_SETTABSTOPS:                 name := 'EM_SETTABSTOPS';
        EM_SETPASSWORDCHAR:             name := 'EM_SETPASSWORDCHAR';
        EM_EMPTYUNDOBUFFER:             name := 'EM_EMPTYUNDOBUFFER';
        EM_GETFIRSTVISIBLELINE:         name := 'EM_GETFIRSTVISIBLELINE ';
        EM_SETREADONLY:                 name := 'EM_SETREADONLY';
        EM_SETWORDBREAKPROC:            name := 'EM_SETWORDBREAKPROC';
        EM_GETWORDBREAKPROC:            name := 'EM_GETWORDBREAKPROC';
        EM_GETPASSWORDCHAR:             name := 'EM_GETPASSWORDCHAR';
        EM_SETMARGINS:                  name := 'EM_SETMARGINS';
        EM_GETMARGINS:                  name := 'EM_GETMARGINS';
        EM_GETLIMITTEXT:                name := 'EM_GETLIMITTEXT';
        EM_POSFROMCHAR:                 name := 'EM_POSFROMCHAR';
        EM_CHARFROMPOS:                 name := 'EM_CHARFROMPOS';
        $d8:                            name := 'EM_SETIMESTATUS';
        $d9:                            name := 'EM_GETIMESTATUS';
        SBM_SETPOS:                     name := 'SBM_SETPOS';
        SBM_GETPOS:                     name := 'SBM_GETPOS';
        SBM_SETRANGE:                   name := 'SBM_SETRANGE';
        SBM_GETRANGE:                   name := 'SBM_GETRANGE';
        SBM_ENABLE_ARROWS:              name := 'SBM_ENABLE_ARROWS';
        SBM_SETRANGEREDRAW:             name := 'SBM_SETRANGEREDRAW';
        SBM_SETSCROLLINFO:              name := 'SBM_SETSCROLLINFO';
        SBM_GETSCROLLINFO:              name := 'SBM_GETSCROLLINFO';
        $eb:                            name := 'SBM_GETSCROLLBARINFO';
        BM_GETCHECK:                    name := 'BM_GETCHECK';
        BM_SETCHECK:                    name := 'BM_SETCHECK';
        BM_GETSTATE:                    name := 'BM_GETSTATE';
        BM_SETSTATE:                    name := 'BM_SETSTATE';
        BM_SETSTYLE:                    name := 'BM_SETSTYLE';
        BM_CLICK:                       name := 'BM_CLICK';
        BM_GETIMAGE:                    name := 'BM_GETIMAGE';
        BM_SETIMAGE:                    name := 'BM_SETIMAGE';
        BM_SETDONTCLICK:                name := 'BM_SETDONTCLICK';
        WM_INPUT:                       name := 'WM_INPUT';
        WM_KEYDOWN:                     name := 'WM_KEYDOWN or WM_KEYFIRST';
        WM_KEYUP:                       name := 'WM_KEYUP';
        WM_CHAR:                        name := 'WM_CHAR';
        WM_DEADCHAR:                    name := 'WM_DEADCHAR';
        WM_SYSKEYDOWN:                  name := 'WM_SYSKEYDOWN';
        WM_SYSKEYUP:                    name := 'WM_SYSKEYUP';
        WM_SYSCHAR:                     name := 'WM_SYSCHAR';
        WM_SYSDEADCHAR:                 name := 'WM_SYSDEADCHAR';
        $108:                           name := 'WM_KEYLAST';
        $109:                           name := 'WM_UNICHAR or WM_WNT_CONVERTREQUESTEX';
        $10a:                           name := 'WM_CONVERTREQUEST';
        $10b:                           name := 'WM_CONVERTRESULT';
        $10c:                           name := 'WM_INTERIM';
        WM_IME_STARTCOMPOSITION:        name := 'WM_IME_STARTCOMPOSITION';
        WM_IME_ENDCOMPOSITION:          name := 'WM_IME_ENDCOMPOSITION';
        WM_IME_COMPOSITION:             name := 'WM_IME_COMPOSITION or WM_IME_KEYLAST';
        WM_INITDIALOG:                  name := 'WM_INITDIALOG';
        WM_COMMAND:                     name := 'WM_COMMAND';
        WM_SYSCOMMAND:                  name := 'WM_SYSCOMMAND';
        WM_TIMER:                       name := 'WM_TIMER';
        WM_HSCROLL:                     name := 'WM_HSCROLL';
        WM_VSCROLL:                     name := 'WM_VSCROLL';
        WM_INITMENU:                    name := 'WM_INITMENU';
        WM_INITMENUPOPUP:               name := 'WM_INITMENUPOPUP';
        $118:                           name := 'WM_SYSTIMER';
        WM_MENUSELECT:                  name := 'WM_MENUSELECT';
        WM_MENUCHAR:                    name := 'WM_MENUCHAR';
        WM_ENTERIDLE:                   name := 'WM_ENTERIDLE';
        WM_MENURBUTTONUP:               name := 'WM_MENURBUTTONUP';
        WM_MENUDRAG:                    name := 'WM_MENUDRAG';
        WM_MENUGETOBJECT:               name := 'WM_MENUGETOBJECT';
        WM_UNINITMENUPOPUP:             name := 'WM_UNINITMENUPOPUP';
        WM_MENUCOMMAND:                 name := 'WM_MENUCOMMAND';
        WM_CHANGEUISTATE:               name := 'WM_CHANGEUISTATE';
        WM_UPDATEUISTATE:               name := 'WM_UPDATEUISTATE';
        WM_QUERYUISTATE:                name := 'WM_QUERYUISTATE';
        WM_CTLCOLORMSGBOX:              name := 'WM_CTLCOLORMSGBOX';
        WM_CTLCOLOREDIT:                name := 'WM_CTLCOLOREDIT';
        WM_CTLCOLORLISTBOX:             name := 'WM_CTLCOLORLISTBOX';
        WM_CTLCOLORBTN:                 name := 'WM_CTLCOLORBTN';
        WM_CTLCOLORDLG:                 name := 'WM_CTLCOLORDLG';
        WM_CTLCOLORSCROLLBAR:           name := 'WM_CTLCOLORSCROLLBAR';
        WM_CTLCOLORSTATIC:              name := 'WM_CTLCOLORSTATIC';
        WM_MOUSEMOVE:                   name := 'WM_MOUSEMOVE or WM_MOUSEFIRST';
        WM_LBUTTONDOWN:                 name := 'WM_LBUTTONDOWN';
        WM_LBUTTONUP:                   name := 'WM_LBUTTONUP';
        WM_LBUTTONDBLCLK:               name := 'WM_LBUTTONDBLCLK';
        WM_RBUTTONDOWN:                 name := 'WM_RBUTTONDOWN';
        WM_RBUTTONUP:                   name := 'WM_RBUTTONUP';
        WM_RBUTTONDBLCLK:               name := 'WM_RBUTTONDBLCLK';
        WM_MBUTTONDOWN:                 name := 'WM_MBUTTONDOWN';
        WM_MBUTTONUP:                   name := 'WM_MBUTTONUP';
        WM_MBUTTONDBLCLK:               name := 'WM_MBUTTONDBLCLK or WM_MOUSELAST';
        WM_MOUSEWHEEL:                  name := 'WM_MOUSEWHEEL';
        WM_XBUTTONDOWN:                 name := 'WM_XBUTTONDOWN';
        WM_XBUTTONUP:                   name := 'WM_XBUTTONUP';
        WM_XBUTTONDBLCLK:               name := 'WM_XBUTTONDBLCLK';
        WM_PARENTNOTIFY:                name := 'WM_PARENTNOTIFY';
        WM_ENTERMENULOOP:               name := 'WM_ENTERMENULOOP';
        WM_EXITMENULOOP:                name := 'WM_EXITMENULOOP';
        WM_NEXTMENU:                    name := 'WM_NEXTMENU';
        WM_SIZING:                      name := 'WM_SIZING';
        WM_CAPTURECHANGED:              name := 'WM_CAPTURECHANGED';
        WM_MOVING:                      name := 'WM_MOVING';
        WM_POWERBROADCAST:              name := 'WM_POWERBROADCAST';
        WM_DEVICECHANGE:                name := 'WM_DEVICECHANGE';
        WM_MDICREATE:                   name := 'WM_MDICREATE';
        WM_MDIDESTROY:                  name := 'WM_MDIDESTROY';
        WM_MDIACTIVATE:                 name := 'WM_MDIACTIVATE';
        WM_MDIRESTORE:                  name := 'WM_MDIRESTORE';
        WM_MDINEXT:                     name := 'WM_MDINEXT';
        WM_MDIMAXIMIZE:                 name := 'WM_MDIMAXIMIZE';
        WM_MDITILE:                     name := 'WM_MDITILE';
        WM_MDICASCADE:                  name := 'WM_MDICASCADE';
        WM_MDIICONARRANGE:              name := 'WM_MDIICONARRANGE';
        WM_MDIGETACTIVE:                name := 'WM_MDIGETACTIVE';
        WM_MDISETMENU:                  name := 'WM_MDISETMENU';
        WM_ENTERSIZEMOVE:               name := 'WM_ENTERSIZEMOVE';
        WM_EXITSIZEMOVE:                name := 'WM_EXITSIZEMOVE';
        WM_DROPFILES:                   name := 'WM_DROPFILES';
        WM_MDIREFRESHMENU:              name := 'WM_MDIREFRESHMENU';
        $280:                           name := 'WM_IME_REPORT';
        WM_IME_SETCONTEXT:              name := 'WM_IME_SETCONTEXT';
        WM_IME_NOTIFY:                  name := 'WM_IME_NOTIFY';
        WM_IME_CONTROL:                 name := 'WM_IME_CONTROL';
        WM_IME_COMPOSITIONFULL:         name := 'WM_IME_COMPOSITIONFULL';
        WM_IME_SELECT:                  name := 'WM_IME_SELECT';
        WM_IME_CHAR:                    name := 'WM_IME_CHAR';
        WM_IME_REQUEST:                 name := 'WM_IME_REQUEST';
        WM_IME_KEYDOWN:                 name := 'WM_IME_KEYDOWN';
        WM_IME_KEYUP:                   name := 'WM_IME_KEYUP';
        WM_NCMOUSEHOVER:                name := 'WM_NCMOUSEHOVER';
        WM_MOUSEHOVER:                  name := 'WM_MOUSEHOVER';
        WM_NCMOUSELEAVE:                name := 'WM_NCMOUSELEAVE';
        WM_MOUSELEAVE:                  name := 'WM_MOUSELEAVE';
        WM_CUT:                         name := 'WM_CUT';
        WM_COPY:                        name := 'WM_COPY';
        WM_PASTE:                       name := 'WM_PASTE';
        WM_CLEAR:                       name := 'WM_CLEAR';
        WM_UNDO:                        name := 'WM_UNDO';
        WM_RENDERFORMAT:                name := 'WM_RENDERFORMAT';
        WM_RENDERALLFORMATS:            name := 'WM_RENDERALLFORMATS';
        WM_DESTROYCLIPBOARD:            name := 'WM_DESTROYCLIPBOARD';
        WM_DRAWCLIPBOARD:               name := 'WM_DRAWCLIPBOARD';
        WM_PAINTCLIPBOARD:              name := 'WM_PAINTCLIPBOARD';
        WM_VSCROLLCLIPBOARD:            name := 'WM_VSCROLLCLIPBOARD';
        WM_SIZECLIPBOARD:               name := 'WM_SIZECLIPBOARD';
        WM_ASKCBFORMATNAME:             name := 'WM_ASKCBFORMATNAME';
        WM_CHANGECBCHAIN:               name := 'WM_CHANGECBCHAIN';
        WM_HSCROLLCLIPBOARD:            name := 'WM_HSCROLLCLIPBOARD';
        WM_QUERYNEWPALETTE:             name := 'WM_QUERYNEWPALETTE';
        WM_PALETTEISCHANGING:           name := 'WM_PALETTEISCHANGING';
        WM_PALETTECHANGED:              name := 'WM_PALETTECHANGED';
        WM_HOTKEY:                      name := 'WM_HOTKEY';
        WM_PRINT:                       name := 'WM_PRINT';
        WM_PRINTCLIENT:                 name := 'WM_PRINTCLIENT';
        WM_APPCOMMAND:                  name := 'WM_APPCOMMAND';
        $358:                           name := 'WM_HANDHELDFIRST';
        $35f:                           name := 'WM_HANDHELDLAST';
        $360:                           name := 'WM_AFXFIRST';
        $37f:                           name := 'WM_AFXLAST';
        WM_PENWINFIRST:                 name := 'WM_PENWINFIRST';
        $381:                           name := 'WM_RCRESULT';
        $382:                           name := 'WM_HOOKRCRESULT';
        $383:                           name := 'WM_GLOBALRCCHANGE or WM_PENMISCINFO';
        $384:                           name := 'WM_SKB';
        $385:                           name := 'WM_HEDITCTL or WM_PENCTL';
        $386:                           name := 'WM_PENMISC';
        $387:                           name := 'WM_CTLINIT';
        $388:                           name := 'WM_PENEVENT';
        WM_PENWINLAST:                  name := 'WM_PENWINLAST';
        $400:                           name := 'DDM_SETFMT or DM_GETDEFID or NIN_SELECT or TBM_GETPOS or WM_PSD_PAGESETUPDLG or WM_USER';
        $401:                           name := 'CBEM_INSERTITEMA or DDM_DRAW or DM_SETDEFID or HKM_SETHOTKEY or PBM_SETRANGE or RB_INSERTBANDA or SB_SETTEXTA or TB_ENABLEBUTTON or TBM_GETRANGEMIN or TTM_ACTIVATE or WM_CHOOSEFONT_GETLOGFONT or WM_PSD_FULLPAGERECT';
        $402:                           name := 'CBEM_SETIMAGELIST or DDM_CLOSE or DM_REPOSITION or HKM_GETHOTKEY or PBM_SETPOS or RB_DELETEBAND or SB_GETTEXTA or TB_CHECKBUTTON or TBM_GETRANGEMAX or WM_PSD_MINMARGINRECT';
        $403:                           name := 'CBEM_GETIMAGELIST or DDM_BEGIN or HKM_SETRULES or PBM_DELTAPOS or RB_GETBARINFO or SB_GETTEXTLENGTHA or TBM_GETTIC or TB_PRESSBUTTON or TTM_SETDELAYTIME or WM_PSD_MARGINRECT';
        $404:                           name := 'CBEM_GETITEMA or DDM_END or PBM_SETSTEP or RB_SETBARINFO or SB_SETPARTS or TB_HIDEBUTTON or TBM_SETTIC or TTM_ADDTOOLA or WM_PSD_GREEKTEXTRECT';
        $405:                           name := 'CBEM_SETITEMA or PBM_STEPIT or TB_INDETERMINATE or TBM_SETPOS or TTM_DELTOOLA or WM_PSD_ENVSTAMPRECT';
        $406:                           name := 'CBEM_GETCOMBOCONTROL or PBM_SETRANGE32 or RB_SETBANDINFOA or SB_GETPARTS or TB_MARKBUTTON or TBM_SETRANGE or TTM_NEWTOOLRECTA or WM_PSD_YAFULLPAGERECT';
        $407:                           name := 'CBEM_GETEDITCONTROL or PBM_GETRANGE or RB_SETPARENT or SB_GETBORDERS or TBM_SETRANGEMIN or TTM_RELAYEVENT';
        $408:                           name := 'CBEM_SETEXSTYLE or PBM_GETPOS or RB_HITTEST or SB_SETMINHEIGHT or TBM_SETRANGEMAX or TTM_GETTOOLINFOA';
        $409:                           name := 'CBEM_GETEXSTYLE or CBEM_GETEXTENDEDSTYLE or PBM_SETBARCOLOR or RB_GETRECT or SB_SIMPLE or TB_ISBUTTONENABLED or TBM_CLEARTICS or TTM_SETTOOLINFOA';
        $40a:                           name := 'CBEM_HASEDITCHANGED or RB_INSERTBANDW or SB_GETRECT or TB_ISBUTTONCHECKED or TBM_SETSEL or TTM_HITTESTA or WIZ_QUERYNUMPAGES';
        $40b:                           name := 'CBEM_INSERTITEMW or RB_SETBANDINFOW or SB_SETTEXTW or TB_ISBUTTONPRESSED or TBM_SETSELSTART or TTM_GETTEXTA or WIZ_NEXT';
        $40c:                           name := 'CBEM_SETITEMW or RB_GETBANDCOUNT or SB_GETTEXTLENGTHW or TB_ISBUTTONHIDDEN or TBM_SETSELEND or TTM_UPDATETIPTEXTA or WIZ_PREV';
        $40d:                           name := 'CBEM_GETITEMW or RB_GETROWCOUNT or SB_GETTEXTW or TB_ISBUTTONINDETERMINATE or TTM_GETTOOLCOUNT';
        $40e:                           name := 'CBEM_SETEXTENDEDSTYLE or RB_GETROWHEIGHT or SB_ISSIMPLE or TB_ISBUTTONHIGHLIGHTED or TBM_GETPTICS or TTM_ENUMTOOLSA';
        $40f:                           name := 'SB_SETICON or TBM_GETTICPOS or TTM_GETCURRENTTOOLA';
        $410:                           name := 'RB_IDTOINDEX or SB_SETTIPTEXTA or TBM_GETNUMTICS or TTM_WINDOWFROMPOINT';
        $411:                           name := 'RB_GETTOOLTIPS or SB_SETTIPTEXTW or TBM_GETSELSTART or TB_SETSTATE or TTM_TRACKACTIVATE';
        $412:                           name := 'RB_SETTOOLTIPS or SB_GETTIPTEXTA or TB_GETSTATE or TBM_GETSELEND or TTM_TRACKPOSITION';
        $413:                           name := 'RB_SETBKCOLOR or SB_GETTIPTEXTW or TB_ADDBITMAP or TBM_CLEARSEL or TTM_SETTIPBKCOLOR';
        $414:                           name := 'RB_GETBKCOLOR or SB_GETICON or TB_ADDBUTTONSA or TBM_SETTICFREQ or TTM_SETTIPTEXTCOLOR';
        $415:                           name := 'RB_SETTEXTCOLOR or TB_INSERTBUTTONA or TBM_SETPAGESIZE or TTM_GETDELAYTIME';
        $416:                           name := 'RB_GETTEXTCOLOR or TB_DELETEBUTTON or TBM_GETPAGESIZE or TTM_GETTIPBKCOLOR';
        $417:                           name := 'RB_SIZETORECT or TB_GETBUTTON or TBM_SETLINESIZE or TTM_GETTIPTEXTCOLOR';
        $418:                           name := 'RB_BEGINDRAG or TB_BUTTONCOUNT or TBM_GETLINESIZE or TTM_SETMAXTIPWIDTH';
        $419:                           name := 'RB_ENDDRAG or TB_COMMANDTOINDEX or TBM_GETTHUMBRECT or TTM_GETMAXTIPWIDTH';
        $41a:                           name := 'RB_DRAGMOVE or TBM_GETCHANNELRECT or TB_SAVERESTOREA or TTM_SETMARGIN';
        $41b:                           name := 'RB_GETBARHEIGHT or TB_CUSTOMIZE or TBM_SETTHUMBLENGTH or TTM_GETMARGIN';
        $41c:                           name := 'RB_GETBANDINFOW or TB_ADDSTRINGA or TBM_GETTHUMBLENGTH or TTM_POP';
        $41d:                           name := 'RB_GETBANDINFOA or TB_GETITEMRECT or TBM_SETTOOLTIPS or TTM_UPDATE';
        $41e:                           name := 'RB_MINIMIZEBAND or TB_BUTTONSTRUCTSIZE or TBM_GETTOOLTIPS or TTM_GETBUBBLESIZE';
        $41f:                           name := 'RB_MAXIMIZEBAND or TBM_SETTIPSIDE or TB_SETBUTTONSIZE or TTM_ADJUSTRECT';
        $420:                           name := 'TBM_SETBUDDY or TB_SETBITMAPSIZE or TTM_SETTITLEA';
        $421:                           name := 'MSG_FTS_JUMP_VA or TB_AUTOSIZE or TBM_GETBUDDY or TTM_SETTITLEW';
        $422:                           name := 'RB_GETBANDBORDERS';
        $423:                           name := 'MSG_FTS_JUMP_QWORD or RB_SHOWBAND or TB_GETTOOLTIPS';
        $424:                           name := 'MSG_REINDEX_REQUEST or TB_SETTOOLTIPS';
        $425:                           name := 'MSG_FTS_WHERE_IS_IT or RB_SETPALETTE or TB_SETPARENT';
        $426:                           name := 'RB_GETPALETTE';
        $427:                           name := 'RB_MOVEBAND or TB_SETROWS';
        $428:                           name := 'TB_GETROWS';
        $429:                           name := 'TB_GETBITMAPFLAGS';
        $42a:                           name := 'TB_SETCMDID';
        $42b:                           name := 'RB_PUSHCHEVRON or TB_CHANGEBITMAP';
        $42c:                           name := 'TB_GETBITMAP';
        $42d:                           name := 'MSG_GET_DEFFONT or TB_GETBUTTONTEXTA';
        $42e:                           name := 'TB_REPLACEBITMAP';
        $42f:                           name := 'TB_SETINDENT';
        $430:                           name := 'TB_SETIMAGELIST';
        $431:                           name := 'TB_GETIMAGELIST';
        $432:                           name := 'TB_LOADIMAGES or EM_CANPASTE or TTM_ADDTOOLW';
        $433:                           name := 'EM_DISPLAYBAND or TB_GETRECT or TTM_DELTOOLW';
        $434:                           name := 'EM_EXGETSEL or TB_SETHOTIMAGELIST or TTM_NEWTOOLRECTW';
        $435:                           name := 'EM_EXLIMITTEXT or TB_GETHOTIMAGELIST or TTM_GETTOOLINFOW';
        $436:                           name := 'EM_EXLINEFROMCHAR or TB_SETDISABLEDIMAGELIST or TTM_SETTOOLINFOW';
        $437:                           name := 'EM_EXSETSEL or TB_GETDISABLEDIMAGELIST or TTM_HITTESTW';
        $438:                           name := 'EM_FINDTEXT or TB_SETSTYLE or TTM_GETTEXTW';
        $439:                           name := 'EM_FORMATRANGE or TB_GETSTYLE or TTM_UPDATETIPTEXTW';
        $43a:                           name := 'EM_GETCHARFORMAT or TB_GETBUTTONSIZE or TTM_ENUMTOOLSW';
        $43b:                           name := 'EM_GETEVENTMASK or TB_SETBUTTONWIDTH or TTM_GETCURRENTTOOLW';
        $43c:                           name := 'EM_GETOLEINTERFACE or TB_SETMAXTEXTROWS';
        $43d:                           name := 'EM_GETPARAFORMAT or TB_GETTEXTROWS';
        $43e:                           name := 'EM_GETSELTEXT or TB_GETOBJECT';
        $43f:                           name := 'EM_HIDESELECTION or TB_GETBUTTONINFOW';
        $440:                           name := 'EM_PASTESPECIAL or TB_SETBUTTONINFOW';
        $441:                           name := 'EM_REQUESTRESIZE or TB_GETBUTTONINFOA';
        $442:                           name := 'EM_SELECTIONTYPE or TB_SETBUTTONINFOA';
        $443:                           name := 'EM_SETBKGNDCOLOR or TB_INSERTBUTTONW';
        $444:                           name := 'EM_SETCHARFORMAT or TB_ADDBUTTONSW';
        $445:                           name := 'EM_SETEVENTMASK or TB_HITTEST';
        $446:                           name := 'EM_SETOLECALLBACK or TB_SETDRAWTEXTFLAGS';
        $447:                           name := 'EM_SETPARAFORMAT or TB_GETHOTITEM';
        $448:                           name := 'EM_SETTARGETDEVICE or TB_SETHOTITEM';
        $449:                           name := 'EM_STREAMIN or TB_SETANCHORHIGHLIGHT';
        $44a:                           name := 'EM_STREAMOUT or TB_GETANCHORHIGHLIGHT';
        $44b:                           name := 'EM_GETTEXTRANGE or TB_GETBUTTONTEXTW';
        $44c:                           name := 'EM_FINDWORDBREAK or TB_SAVERESTOREW';
        $44d:                           name := 'EM_SETOPTIONS or TB_ADDSTRINGW';
        $44e:                           name := 'EM_GETOPTIONS or TB_MAPACCELERATORA';
        $44f:                           name := 'EM_FINDTEXTEX or TB_GETINSERTMARK';
        $450:                           name := 'EM_GETWORDBREAKPROCEX or TB_SETINSERTMARK';
        $451:                           name := 'EM_SETWORDBREAKPROCEX or TB_INSERTMARKHITTEST';
        $452:                           name := 'EM_SETUNDOLIMIT or TB_MOVEBUTTON';
        $453:                           name := 'TB_GETMAXSIZE';
        $454:                           name := 'EM_REDO or TB_SETEXTENDEDSTYLE';
        $455:                           name := 'EM_CANREDO or TB_GETEXTENDEDSTYLE';
        $456:                           name := 'EM_GETUNDONAME or TB_GETPADDING';
        $457:                           name := 'EM_GETREDONAME or TB_SETPADDING';
        $458:                           name := 'EM_STOPGROUPTYPING or TB_SETINSERTMARKCOLOR';
        $459:                           name := 'EM_SETTEXTMODE or TB_GETINSERTMARKCOLOR';
        $45a:                           name := 'EM_GETTEXTMODE or TB_MAPACCELERATORW';
        $45b:                           name := 'EM_AUTOURLDETECT or TB_GETSTRINGW';
        $45c:                           name := 'EM_GETAUTOURLDETECT or TB_GETSTRINGA';
        $45d:                           name := 'EM_SETPALETTE';
        $45e:                           name := 'EM_GETTEXTEX';
        $45f:                           name := 'EM_GETTEXTLENGTHEX';
        $460:                           name := 'EM_SHOWSCROLLBAR';
        $461:                           name := 'EM_SETTEXTEX';
        $463:                           name := 'TAPI_REPLY';
        $464:                           name := 'ACM_OPENA or BFFM_SSETSTATUSTEXTA or CDM_FIRST or CDM_GETSPEC or EM_SETPUNCTUATION or IPM_CLEARADDRESS or WM_CAP_UNICODE_START';
        $465:                           name := 'ACM_PLAY or BFFM_SENABLEOK or CDM_GETFILEPATH or EM_GETPUNCTUATION or IPM_SETADDRESS or PSM_SETCURSEL or UDM_SETRANGE or WM_CHOOSEFONT_SETLOGFONT';
        $466:                           name := 'ACM_STOP or BFFM_SSETSELECTIONA or CDM_GETFOLDERPATH or EM_SETWORDWRAPMODE or IPM_GETADDRESS or PSM_REMOVEPAGE or UDM_GETRANGE or WM_CAP_SET_CALLBACK_ERRORW or WM_CHOOSEFONT_SETFLAGS';
        $467:                           name := 'ACM_OPENW or BFFM_SSETSELECTIONW or CDM_GETFOLDERIDLIST or EM_GETWORDWRAPMODE or IPM_SETRANGE or PSM_ADDPAGE or UDM_SETPOS or WM_CAP_SET_CALLBACK_STATUSW';
        $468:                           name := 'BFFM_SSETSTATUSTEXTW or CDM_SETCONTROLTEXT or EM_SETIMECOLOR or IPM_SETFOCUS or PSM_CHANGED or UDM_GETPOS';
        $469:                           name := 'CDM_HIDECONTROL or EM_GETIMECOLOR or IPM_ISBLANK or PSM_RESTARTWINDOWS or UDM_SETBUDDY';
        $46a:                           name := 'CDM_SETDEFEXT or EM_SETIMEOPTIONS or PSM_REBOOTSYSTEM or UDM_GETBUDDY';
        $46b:                           name := 'EM_GETIMEOPTIONS or PSM_CANCELTOCLOSE or UDM_SETACCEL';
        $46c:                           name := 'EM_CONVPOSITION or EM_CONVPOSITION or PSM_QUERYSIBLINGS or UDM_GETACCEL';
        $46d:                           name := 'MCIWNDM_GETZOOM or PSM_UNCHANGED or UDM_SETBASE';
        $46e:                           name := 'PSM_APPLY or UDM_GETBASE';
        $46f:                           name := 'PSM_SETTITLEA or UDM_SETRANGE32';
        $470:                           name := 'PSM_SETWIZBUTTONS or UDM_GETRANGE32 or WM_CAP_DRIVER_GET_NAMEW';
        $471:                           name := 'PSM_PRESSBUTTON or UDM_SETPOS32 or WM_CAP_DRIVER_GET_VERSIONW';
        $472:                           name := 'PSM_SETCURSELID or UDM_GETPOS32';
        $473:                           name := 'PSM_SETFINISHTEXTA';
        $474:                           name := 'PSM_GETTABCONTROL';
        $475:                           name := 'PSM_ISDIALOGMESSAGE';
        $476:                           name := 'MCIWNDM_REALIZE or PSM_GETCURRENTPAGEHWND';
        $477:                           name := 'MCIWNDM_SETTIMEFORMATA or PSM_INSERTPAGE';
        $478:                           name := 'EM_SETLANGOPTIONS or MCIWNDM_GETTIMEFORMATA or PSM_SETTITLEW or WM_CAP_FILE_SET_CAPTURE_FILEW';
        $479:                           name := 'EM_GETLANGOPTIONS or MCIWNDM_VALIDATEMEDIA or PSM_SETFINISHTEXTW or WM_CAP_FILE_GET_CAPTURE_FILEW';
        $47a:                           name := 'EM_GETIMECOMPMODE';
        $47b:                           name := 'EM_FINDTEXTW or MCIWNDM_PLAYTO or WM_CAP_FILE_SAVEASW';
        $47c:                           name := 'EM_FINDTEXTEXW or MCIWNDM_GETFILENAMEA';
        $47d:                           name := 'EM_RECONVERSION or MCIWNDM_GETDEVICEA or PSM_SETHEADERTITLEA or WM_CAP_FILE_SAVEDIBW';
        $47e:                           name := 'EM_SETIMEMODEBIAS or MCIWNDM_GETPALETTE or PSM_SETHEADERTITLEW';
        $47f:                           name := 'EM_GETIMEMODEBIAS or MCIWNDM_SETPALETTE or PSM_SETHEADERSUBTITLEA';
        $480:                           name := 'MCIWNDM_GETERRORA or PSM_SETHEADERSUBTITLEW';
        $481:                           name := 'PSM_HWNDTOINDEX';
        $482:                           name := 'PSM_INDEXTOHWND';
        $483:                           name := 'MCIWNDM_SETINACTIVETIMER or PSM_PAGETOINDEX';
        $484:                           name := 'PSM_INDEXTOPAGE';
        $485:                           name := 'DL_BEGINDRAG or MCIWNDM_GETINACTIVETIMER or PSM_IDTOINDEX';
        $486:                           name := 'DL_DRAGGING or PSM_INDEXTOID';
        $487:                           name := 'DL_DROPPED or PSM_GETRESULT';
        $488:                           name := 'DL_CANCELDRAG or PSM_RECALCPAGESIZES';
        $48c:                           name := 'MCIWNDM_GET_SOURCE';
        $48d:                           name := 'MCIWNDM_PUT_SOURCE';
        $48e:                           name := 'MCIWNDM_GET_DEST';
        $48f:                           name := 'MCIWNDM_PUT_DEST';
        $490:                           name := 'MCIWNDM_CAN_PLAY';
        $491:                           name := 'MCIWNDM_CAN_WINDOW';
        $492:                           name := 'MCIWNDM_CAN_RECORD';
        $493:                           name := 'MCIWNDM_CAN_SAVE';
        $494:                           name := 'MCIWNDM_CAN_EJECT';
        $495:                           name := 'MCIWNDM_CAN_CONFIG';
        $496:                           name := 'IE_GETINK or IE_MSGFIRST or MCIWNDM_PALETTEKICK';
        $497:                           name := 'IE_SETINK';
        $498:                           name := 'IE_GETPENTIP';
        $499:                           name := 'IE_SETPENTIP';
        $49a:                           name := 'IE_GETERASERTIP';
        $49b:                           name := 'IE_SETERASERTIP';
        $49c:                           name := 'IE_GETBKGND';
        $49d:                           name := 'IE_SETBKGND';
        $49e:                           name := 'IE_GETGRIDORIGIN';
        $49f:                           name := 'IE_SETGRIDORIGIN';
        $4a0:                           name := 'IE_GETGRIDPEN';
        $4a1:                           name := 'IE_SETGRIDPEN';
        $4a2:                           name := 'IE_GETGRIDSIZE';
        $4a3:                           name := 'IE_SETGRIDSIZE';
        $4a4:                           name := 'IE_GETMODE';
        $4a5:                           name := 'IE_SETMODE';
        $4a6:                           name := 'IE_GETINKRECT or WM_CAP_SET_MCI_DEVICEW';
        $4a7:                           name := 'WM_CAP_GET_MCI_DEVICEW';
        $4b4:                           name := 'WM_CAP_PAL_OPENW';
        $4b5:                           name := 'WM_CAP_PAL_SAVEW';
        $4b8:                           name := 'IE_GETAPPDATA';
        $4b9:                           name := 'IE_SETAPPDATA';
        $4ba:                           name := 'IE_GETDRAWOPTS';
        $4bb:                           name := 'IE_SETDRAWOPTS';
        $4bc:                           name := 'IE_GETFORMAT';
        $4bd:                           name := 'IE_SETFORMAT';
        $4be:                           name := 'IE_GETINKINPUT';
        $4bf:                           name := 'IE_SETINKINPUT';
        $4c0:                           name := 'IE_GETNOTIFY';
        $4c1:                           name := 'IE_SETNOTIFY';
        $4c2:                           name := 'IE_GETRECOG';
        $4c3:                           name := 'IE_SETRECOG';
        $4c4:                           name := 'IE_GETSECURITY';
        $4c5:                           name := 'IE_SETSECURITY';
        $4c6:                           name := 'IE_GETSEL';
        $4c7:                           name := 'IE_SETSEL';
        $4c8:                           name := 'CDM_LAST or EM_SETBIDIOPTIONS or IE_DOCOMMAND or MCIWNDM_NOTIFYMODE';
        $4c9:                           name := 'EM_GETBIDIOPTIONS or IE_GETCOMMAND';
        $4ca:                           name := 'EM_SETTYPOGRAPHYOPTIONS or IE_GETCOUNT';
        $4cb:                           name := 'EM_GETTYPOGRAPHYOPTIONS or IE_GETGESTURE or MCIWNDM_NOTIFYMEDIA';
        $4cc:                           name := 'EM_SETEDITSTYLE or IE_GETMENU';
        $4cd:                           name := 'EM_GETEDITSTYLE or IE_GETPAINTDC or MCIWNDM_NOTIFYERROR';
        $4ce:                           name := 'IE_GETPDEVENT';
        $4cf:                           name := 'IE_GETSELCOUNT';
        $4d0:                           name := 'IE_GETSELITEMS';
        $4d1:                           name := 'IE_GETSTYLE';
        $4db:                           name := 'MCIWNDM_SETTIMEFORMATW';
        $4dc:                           name := 'EM_OUTLINE or MCIWNDM_GETTIMEFORMATW';
        $4dd:                           name := 'EM_GETSCROLLPOS';
        $4de:                           name := 'EM_SETSCROLLPOS';
        $4df:                           name := 'EM_SETFONTSIZE';
        $4e0:                           name := 'EM_GETZOOM or MCIWNDM_GETFILENAMEW';
        $4e1:                           name := 'EM_SETZOOM or MCIWNDM_GETDEVICEW';
        $4e2:                           name := 'EM_GETVIEWKIND';
        $4e3:                           name := 'EM_SETVIEWKIND';
        $4e4:                           name := 'EM_GETPAGE or MCIWNDM_GETERRORW';
        $4e5:                           name := 'EM_SETPAGE';
        $4e6:                           name := 'EM_GETHYPHENATEINFO';
        $4e7:                           name := 'EM_SETHYPHENATEINFO';
        $4eb:                           name := 'EM_GETPAGEROTATE';
        $4ec:                           name := 'EM_SETPAGEROTATE';
        $4ed:                           name := 'EM_GETCTFMODEBIAS';
        $4ee:                           name := 'EM_SETCTFMODEBIAS';
        $4f0:                           name := 'EM_GETCTFOPENSTATUS';
        $4f1:                           name := 'EM_SETCTFOPENSTATUS';
        $4f2:                           name := 'EM_GETIMECOMPTEXT';
        $4f3:                           name := 'EM_ISIME';
        $4f4:                           name := 'EM_GETIMEPROPERTY';
        $50d:                           name := 'EM_GETQUERYRTFOBJ';
        $50e:                           name := 'EM_SETQUERYRTFOBJ';
        $600:                           name := 'FM_GETFOCUS';
        $601:                           name := 'FM_GETDRIVEINFOA';
        $602:                           name := 'FM_GETSELCOUNT';
        $603:                           name := 'FM_GETSELCOUNTLFN';
        $604:                           name := 'FM_GETFILESELA';
        $605:                           name := 'FM_GETFILESELLFNA';
        $606:                           name := 'FM_REFRESH_WINDOWS';
        $607:                           name := 'FM_RELOAD_EXTENSIONS';
        $611:                           name := 'FM_GETDRIVEINFOW';
        $614:                           name := 'FM_GETFILESELW';
        $615:                           name := 'FM_GETFILESELLFNW';
        $659:                           name := 'WLX_WM_SAS';
        $7e8:                           name := 'SM_GETSELCOUNT or UM_GETSELCOUNT or WM_CPL_LAUNCH';
        $7e9:                           name := 'SM_GETSERVERSELA or UM_GETUSERSELA or WM_CPL_LAUNCHED';
        $7ea:                           name := 'SM_GETSERVERSELW or UM_GETUSERSELW';
        $7eb:                           name := 'SM_GETCURFOCUSA or UM_GETGROUPSELA';
        $7ec:                           name := 'SM_GETCURFOCUSW or UM_GETGROUPSELW';
        $7ed:                           name := 'SM_GETOPTIONS or UM_GETCURFOCUSA';
        $7ee:                           name := 'UM_GETCURFOCUSW';
        $7ef:                           name := 'UM_GETOPTIONS';
        $7f0:                           name := 'UM_GETOPTIONS2';
        $1000:                          name := 'LVM_FIRST or LVM_GETBKCOLOR';
        $1001:                          name := 'LVM_SETBKCOLOR';
        $1002:                          name := 'LVM_GETIMAGELIST';
        $1003:                          name := 'LVM_SETIMAGELIST';
        $1004:                          name := 'LVM_GETITEMCOUNT';
        $1005:                          name := 'LVM_GETITEMA';
        $1006:                          name := 'LVM_SETITEMA';
        $1007:                          name := 'LVM_INSERTITEMA';
        $1008:                          name := 'LVM_DELETEITEM';
        $1009:                          name := 'LVM_DELETEALLITEMS';
        $100a:                          name := 'LVM_GETCALLBACKMASK';
        $100b:                          name := 'LVM_SETCALLBACKMASK';
        $100c:                          name := 'LVM_GETNEXTITEM';
        $100d:                          name := 'LVM_FINDITEMA';
        $100e:                          name := 'LVM_GETITEMRECT';
        $100f:                          name := 'LVM_SETITEMPOSITION';
        $1010:                          name := 'LVM_GETITEMPOSITION';
        $1011:                          name := 'LVM_GETSTRINGWIDTHA';
        $1012:                          name := 'LVM_HITTEST';
        $1013:                          name := 'LVM_ENSUREVISIBLE';
        $1014:                          name := 'LVM_SCROLL';
        $1015:                          name := 'LVM_REDRAWITEMS';
        $1016:                          name := 'LVM_ARRANGE';
        $1017:                          name := 'LVM_EDITLABELA';
        $1018:                          name := 'LVM_GETEDITCONTROL';
        $1019:                          name := 'LVM_GETCOLUMNA';
        $101a:                          name := 'LVM_SETCOLUMNA';
        $101b:                          name := 'LVM_INSERTCOLUMNA';
        $101c:                          name := 'LVM_DELETECOLUMN';
        $101d:                          name := 'LVM_GETCOLUMNWIDTH';
        $101e:                          name := 'LVM_SETCOLUMNWIDTH';
        $101f:                          name := 'LVM_GETHEADER';
        $1021:                          name := 'LVM_CREATEDRAGIMAGE';
        $1022:                          name := 'LVM_GETVIEWRECT';
        $1023:                          name := 'LVM_GETTEXTCOLOR';
        $1024:                          name := 'LVM_SETTEXTCOLOR';
        $1025:                          name := 'LVM_GETTEXTBKCOLOR';
        $1026:                          name := 'LVM_SETTEXTBKCOLOR';
        $1027:                          name := 'LVM_GETTOPINDEX';
        $1028:                          name := 'LVM_GETCOUNTPERPAGE';
        $1029:                          name := 'LVM_GETORIGIN';
        $102a:                          name := 'LVM_UPDATE';
        $102b:                          name := 'LVM_SETITEMSTATE';
        $102c:                          name := 'LVM_GETITEMSTATE';
        $102d:                          name := 'LVM_GETITEMTEXTA';
        $102e:                          name := 'LVM_SETITEMTEXTA';
        $102f:                          name := 'LVM_SETITEMCOUNT';
        $1030:                          name := 'LVM_SORTITEMS';
        $1031:                          name := 'LVM_SETITEMPOSITION32';
        $1032:                          name := 'LVM_GETSELECTEDCOUNT';
        $1033:                          name := 'LVM_GETITEMSPACING';
        $1034:                          name := 'LVM_GETISEARCHSTRINGA';
        $1035:                          name := 'LVM_SETICONSPACING';
        $1036:                          name := 'LVM_SETEXTENDEDLISTVIEWSTYLE';
        $1037:                          name := 'LVM_GETEXTENDEDLISTVIEWSTYLE';
        $1038:                          name := 'LVM_GETSUBITEMRECT';
        $1039:                          name := 'LVM_SUBITEMHITTEST';
        $103a:                          name := 'LVM_SETCOLUMNORDERARRAY';
        $103b:                          name := 'LVM_GETCOLUMNORDERARRAY';
        $103c:                          name := 'LVM_SETHOTITEM';
        $103d:                          name := 'LVM_GETHOTITEM';
        $103e:                          name := 'LVM_SETHOTCURSOR';
        $103f:                          name := 'LVM_GETHOTCURSOR';
        $1040:                          name := 'LVM_APPROXIMATEVIEWRECT';
        $1041:                          name := 'LVM_SETWORKAREAS';
        $1042:                          name := 'LVM_GETSELECTIONMARK';
        $1043:                          name := 'LVM_SETSELECTIONMARK';
        $1044:                          name := 'LVM_SETBKIMAGEA';
        $1045:                          name := 'LVM_GETBKIMAGEA';
        $1046:                          name := 'LVM_GETWORKAREAS';
        $1047:                          name := 'LVM_SETHOVERTIME';
        $1048:                          name := 'LVM_GETHOVERTIME';
        $1049:                          name := 'LVM_GETNUMBEROFWORKAREAS';
        $104a:                          name := 'LVM_SETTOOLTIPS';
        $104b:                          name := 'LVM_GETITEMW';
        $104c:                          name := 'LVM_SETITEMW';
        $104d:                          name := 'LVM_INSERTITEMW';
        $104e:                          name := 'LVM_GETTOOLTIPS';
        $1053:                          name := 'LVM_FINDITEMW';
        $1057:                          name := 'LVM_GETSTRINGWIDTHW';
        $105f:                          name := 'LVM_GETCOLUMNW';
        $1060:                          name := 'LVM_SETCOLUMNW';
        $1061:                          name := 'LVM_INSERTCOLUMNW';
        $1073:                          name := 'LVM_GETITEMTEXTW';
        $1074:                          name := 'LVM_SETITEMTEXTW';
        $1075:                          name := 'LVM_GETISEARCHSTRINGW';
        $1076:                          name := 'LVM_EDITLABELW';
        $108b:                          name := 'LVM_GETBKIMAGEW';
        $108c:                          name := 'LVM_SETSELECTEDCOLUMN';
        $108d:                          name := 'LVM_SETTILEWIDTH';
        $108e:                          name := 'LVM_SETVIEW';
        $108f:                          name := 'LVM_GETVIEW';
        $1091:                          name := 'LVM_INSERTGROUP';
        $1093:                          name := 'LVM_SETGROUPINFO';
        $1095:                          name := 'LVM_GETGROUPINFO';
        $1096:                          name := 'LVM_REMOVEGROUP';
        $1097:                          name := 'LVM_MOVEGROUP';
        $109a:                          name := 'LVM_MOVEITEMTOGROUP';
        $109b:                          name := 'LVM_SETGROUPMETRICS';
        $109c:                          name := 'LVM_GETGROUPMETRICS';
        $109d:                          name := 'LVM_ENABLEGROUPVIEW';
        $109e:                          name := 'LVM_SORTGROUPS';
        $109f:                          name := 'LVM_INSERTGROUPSORTED';
        $10a0:                          name := 'LVM_REMOVEALLGROUPS';
        $10a1:                          name := 'LVM_HASGROUP';
        $10a2:                          name := 'LVM_SETTILEVIEWINFO';
        $10a3:                          name := 'LVM_GETTILEVIEWINFO';
        $10a4:                          name := 'LVM_SETTILEINFO';
        $10a5:                          name := 'LVM_GETTILEINFO';
        $10a6:                          name := 'LVM_SETINSERTMARK';
        $10a7:                          name := 'LVM_GETINSERTMARK';
        $10a8:                          name := 'LVM_INSERTMARKHITTEST';
        $10a9:                          name := 'LVM_GETINSERTMARKRECT';
        $10aa:                          name := 'LVM_SETINSERTMARKCOLOR';
        $10ab:                          name := 'LVM_GETINSERTMARKCOLOR';
        $10ad:                          name := 'LVM_SETINFOTIP';
        $10ae:                          name := 'LVM_GETSELECTEDCOLUMN';
        $10af:                          name := 'LVM_ISGROUPVIEWENABLED';
        $10b0:                          name := 'LVM_GETOUTLINECOLOR';
        $10b1:                          name := 'LVM_SETOUTLINECOLOR';
        $10b3:                          name := 'LVM_CANCELEDITLABEL';
        $10b4:                          name := 'LVM_MAPINDEXTOID';
        $10b5:                          name := 'LVM_MAPIDTOINDEX';
        $10b6:                          name := 'LVM_ISITEMVISIBLE';
        $2000:                          name := 'OCM_BASE';
        $2005:                          name := 'LVM_SETUNICODEFORMAT';
        $2006:                          name := 'LVM_GETUNICODEFORMAT';
        $2019:                          name := 'OCM_CTLCOLOR';
        $202b:                          name := 'OCM_DRAWITEM';
        $202c:                          name := 'OCM_MEASUREITEM';
        $202d:                          name := 'OCM_DELETEITEM';
        $202e:                          name := 'OCM_VKEYTOITEM';
        $202f:                          name := 'OCM_CHARTOITEM';
        $2039:                          name := 'OCM_COMPAREITEM';
        $204e:                          name := 'OCM_NOTIFY';
        $2111:                          name := 'OCM_COMMAND';
        $2114:                          name := 'OCM_HSCROLL';
        $2115:                          name := 'OCM_VSCROLL';
        $2132:                          name := 'OCM_CTLCOLORMSGBOX';
        $2133:                          name := 'OCM_CTLCOLOREDIT';
        $2134:                          name := 'OCM_CTLCOLORLISTBOX';
        $2135:                          name := 'OCM_CTLCOLORBTN';
        $2136:                          name := 'OCM_CTLCOLORDLG';
        $2137:                          name := 'OCM_CTLCOLORSCROLLBAR';
        $2138:                          name := 'OCM_CTLCOLORSTATIC';
        $2210:                          name := 'OCM_PARENTNOTIFY';
        WM_APP:                         name := 'WM_APP';
        $cccd:                          name := 'WM_RASDIALEVENT';

        // RAD studio messages
        CM_ACTIVATE:                    name := 'CM_ACTIVATE';
        CM_DEACTIVATE:                  name := 'CM_DEACTIVATE';
        CM_GOTFOCUS:                    name := 'CM_GOTFOCUS';
        CM_LOSTFOCUS:                   name := 'CM_LOSTFOCUS';
        CM_CANCELMODE:                  name := 'CM_CANCELMODE';
        CM_DIALOGKEY:                   name := 'CM_DIALOGKEY';
        CM_DIALOGCHAR:                  name := 'CM_DIALOGCHAR';
        CM_FOCUSCHANGED:                name := 'CM_FOCUSCHANGED';
        CM_PARENTFONTCHANGED:           name := 'CM_PARENTFONTCHANGED';
        CM_PARENTCOLORCHANGED:          name := 'CM_PARENTCOLORCHANGED';
        CM_HITTEST:                     name := 'CM_HITTEST';
        CM_VISIBLECHANGED:              name := 'CM_VISIBLECHANGED';
        CM_ENABLEDCHANGED:              name := 'CM_ENABLEDCHANGED';
        CM_COLORCHANGED:                name := 'CM_COLORCHANGED';
        CM_FONTCHANGED:                 name := 'CM_FONTCHANGED';
        CM_CURSORCHANGED:               name := 'CM_CURSORCHANGED';
        CM_CTL3DCHANGED:                name := 'CM_CTL3DCHANGED';
        CM_PARENTCTL3DCHANGED:          name := 'CM_PARENTCTL3DCHANGED';
        CM_TEXTCHANGED:                 name := 'CM_TEXTCHANGED';
        CM_MOUSEENTER:                  name := 'CM_MOUSEENTER';
        CM_MOUSELEAVE:                  name := 'CM_MOUSELEAVE';
        CM_MENUCHANGED:                 name := 'CM_MENUCHANGED';
        CM_APPKEYDOWN:                  name := 'CM_APPKEYDOWN';
        CM_APPSYSCOMMAND:               name := 'CM_APPSYSCOMMAND';
        CM_BUTTONPRESSED:               name := 'CM_BUTTONPRESSED';
        CM_SHOWINGCHANGED:              name := 'CM_SHOWINGCHANGED';
        CM_ENTER:                       name := 'CM_ENTER';
        CM_EXIT:                        name := 'CM_EXIT';
        CM_DESIGNHITTEST:               name := 'CM_DESIGNHITTEST';
        CM_ICONCHANGED:                 name := 'CM_ICONCHANGED';
        CM_WANTSPECIALKEY:              name := 'CM_WANTSPECIALKEY';
        CM_INVOKEHELP:                  name := 'CM_INVOKEHELP';
        CM_WINDOWHOOK:                  name := 'CM_WINDOWHOOK';
        CM_RELEASE:                     name := 'CM_RELEASE';
        CM_SHOWHINTCHANGED:             name := 'CM_SHOWHINTCHANGED';
        CM_PARENTSHOWHINTCHANGED:       name := 'CM_PARENTSHOWHINTCHANGED';
        CM_SYSCOLORCHANGE:              name := 'CM_SYSCOLORCHANGE';
        CM_WININICHANGE:                name := 'CM_WININICHANGE';
        CM_FONTCHANGE:                  name := 'CM_FONTCHANGE';
        CM_TIMECHANGE:                  name := 'CM_TIMECHANGE';
        CM_TABSTOPCHANGED:              name := 'CM_TABSTOPCHANGED';
        CM_UIACTIVATE:                  name := 'CM_UIACTIVATE';
        CM_UIDEACTIVATE:                name := 'CM_UIDEACTIVATE';
        CM_DOCWINDOWACTIVATE:           name := 'CM_DOCWINDOWACTIVATE';
        CM_CONTROLLISTCHANGE:           name := 'CM_CONTROLLISTCHANGE';
        CM_GETDATALINK:                 name := 'CM_GETDATALINK';
        CM_CHILDKEY:                    name := 'CM_CHILDKEY';
        CM_DRAG:                        name := 'CM_DRAG';
        CM_HINTSHOW:                    name := 'CM_HINTSHOW';
        CM_DIALOGHANDLE:                name := 'CM_DIALOGHANDLE';
        CM_ISTOOLCONTROL:               name := 'CM_ISTOOLCONTROL';
        CM_RECREATEWND:                 name := 'CM_RECREATEWND';
        CM_INVALIDATE:                  name := 'CM_INVALIDATE';
        CM_SYSFONTCHANGED:              name := 'CM_SYSFONTCHANGED';
        CM_CONTROLCHANGE:               name := 'CM_CONTROLCHANGE';
        CM_CHANGED:                     name := 'CM_CHANGED';
        CM_DOCKCLIENT:                  name := 'CM_DOCKCLIENT';
        CM_UNDOCKCLIENT:                name := 'CM_UNDOCKCLIENT';
        CM_FLOAT:                       name := 'CM_FLOAT';
        CM_BORDERCHANGED:               name := 'CM_BORDERCHANGED';
        CM_BIDIMODECHANGED:             name := 'CM_BIDIMODECHANGED';
        CM_PARENTBIDIMODECHANGED:       name := 'CM_PARENTBIDIMODECHANGED';
        CM_ALLCHILDRENFLIPPED:          name := 'CM_ALLCHILDRENFLIPPED';
        CM_ACTIONUPDATE:                name := 'CM_ACTIONUPDATE';
        CM_ACTIONEXECUTE:               name := 'CM_ACTIONEXECUTE';
        CM_HINTSHOWPAUSE:               name := 'CM_HINTSHOWPAUSE';
        CM_DOCKNOTIFICATION:            name := 'CM_DOCKNOTIFICATION';
        CM_MOUSEWHEEL:                  name := 'CM_MOUSEWHEEL';
        CM_ISSHORTCUT:                  name := 'CM_ISSHORTCUT';
        CM_UPDATEACTIONS:               name := 'CM_UPDATEACTIONS';
        CM_INVALIDATEDOCKHOST:          name := 'CM_INVALIDATEDOCKHOST';
        CM_SETACTIVECONTROL:            name := 'CM_SETACTIVECONTROL';
        CM_POPUPHWNDDESTROY:            name := 'CM_POPUPHWNDDESTROY';
        CM_CREATEPOPUP:                 name := 'CM_CREATEPOPUP';
        CM_DESTROYHANDLE:               name := 'CM_DESTROYHANDLE';
        CM_MOUSEACTIVATE:               name := 'CM_MOUSEACTIVATE';
        CM_CONTROLLISTCHANGING:         name := 'CM_CONTROLLISTCHANGING';
        CM_BUFFEREDPRINTCLIENT:         name := 'CM_BUFFEREDPRINTCLIENT';
        CM_UNTHEMECONTROL:              name := 'CM_UNTHEMECONTROL';
        CM_DOUBLEBUFFEREDCHANGED:       name := 'CM_DOUBLEBUFFEREDCHANGED';
        CM_PARENTDOUBLEBUFFEREDCHANGED: name := 'CM_PARENTDOUBLEBUFFEREDCHANGED';
        $b051:                          name := 'CM_STYLECHANGED';
        CM_GESTURE:                     name := 'CM_GESTURE';
        CM_CUSTOMGESTURESCHANGED:       name := 'CM_CUSTOMGESTURESCHANGED';
        CM_GESTUREMANAGERCHANGED:       name := 'CM_GESTUREMANAGERCHANGED';
        CM_STANDARDGESTURESCHANGED:     name := 'CM_STANDARDGESTURESCHANGED';
        CM_INPUTLANGCHANGE:             name := 'CM_INPUTLANGCHANGE';
        CM_TABLETOPTIONSCHANGED:        name := 'CM_TABLETOPTIONSCHANGED';
        CM_PARENTTABLETOPTIONSCHANGED:  name := 'CM_PARENTTABLETOPTIONSCHANGED';
        $b059:                          name := 'CM_CUSTOMSTYLECHANGED';

        // RAD Studio VCL control notification IDs
        CN_BASE:                        name := 'CN_BASE';
        CN_CHARTOITEM:                  name := 'CN_CHARTOITEM';
        CN_COMMAND:                     name := 'CN_COMMAND';
        CN_COMPAREITEM:                 name := 'CN_COMPAREITEM';
        CN_CTLCOLORBTN:                 name := 'CN_CTLCOLORBTN';
        CN_CTLCOLORDLG:                 name := 'CN_CTLCOLORDLG';
        CN_CTLCOLOREDIT:                name := 'CN_CTLCOLOREDIT';
        CN_CTLCOLORLISTBOX:             name := 'CN_CTLCOLORLISTBOX';
        CN_CTLCOLORMSGBOX:              name := 'CN_CTLCOLORMSGBOX';
        CN_CTLCOLORSCROLLBAR:           name := 'CN_CTLCOLORSCROLLBAR';
        CN_CTLCOLORSTATIC:              name := 'CN_CTLCOLORSTATIC';
        CN_DELETEITEM:                  name := 'CN_DELETEITEM';
        CN_DRAWITEM:                    name := 'CN_DRAWITEM';
        CN_HSCROLL:                     name := 'CN_HSCROLL';
        CN_MEASUREITEM:                 name := 'CN_MEASUREITEM';
        CN_PARENTNOTIFY:                name := 'CN_PARENTNOTIFY';
        CN_VKEYTOITEM:                  name := 'CN_VKEYTOITEM';
        CN_VSCROLL:                     name := 'CN_VSCROLL';
        CN_KEYDOWN:                     name := 'CN_KEYDOWN';
        CN_KEYUP:                       name := 'CN_KEYUP';
        CN_CHAR:                        name := 'CN_CHAR';
        CN_SYSKEYDOWN:                  name := 'CN_SYSKEYDOWN';
        CN_SYSCHAR:                     name := 'CN_SYSCHAR';
        CN_NOTIFY:                      name := 'CN_NOTIFY';
    else
        // unknown message
        name := 'Unknown [' + UnicodeString(IntToHex(message.Msg, 8)) + ']';
    end;

    // convert wparam, whenever possible
    case (message.Msg) of
        WM_LBUTTONDOWN:
        begin
            if ((message.WParam and MK_CONTROL) = 0) then
                if (Length(wParamName) = 0) then
                    wParamName := 'MK_CONTROL'
                else
                    wParamName := wParamName + ' or MK_CONTROL';

            if ((message.WParam and MK_LBUTTON) = 0) then
                if (Length(wParamName) = 0) then
                    wParamName := 'MK_LBUTTON'
                else
                    wParamName := wParamName + ' or MK_LBUTTON';

            if ((message.WParam and MK_MBUTTON) = 0) then
                if (Length(wParamName) = 0) then
                    wParamName := 'MK_MBUTTON'
                else
                    wParamName := wParamName + ' or MK_MBUTTON';

            if ((message.WParam and MK_MBUTTON) = 0) then
                if (Length(wParamName) = 0) then
                    wParamName := 'MK_RBUTTON'
                else
                    wParamName := wParamName + ' or MK_RBUTTON';

            if ((message.WParam and MK_SHIFT) = 0) then
                if (Length(wParamName) = 0) then
                    wParamName := 'MK_SHIFT'
                else
                    wParamName := wParamName + ' or MK_SHIFT';

            if ((message.WParam and $20) = 0) then
                if (Length(wParamName) = 0) then
                    wParamName := 'MK_XBUTTON1'
                else
                    wParamName := wParamName + ' or MK_XBUTTON1';

            if ((message.WParam and $40) = 0) then
                if (Length(wParamName) = 0) then
                    wParamName := 'MK_XBUTTON2'
                else
                    wParamName := wParamName + ' or MK_XBUTTON2';
        end;
    else
        wParamName := UnicodeString(IntToHex(message.WParam, 8));
    end;

    // convert lparam, whenever possible
    case (message.Msg) of
        WM_LBUTTONDOWN:
            lParamName := 'x <'   + UnicodeString(IntToStr(Integer(ShortInt(LOWORD(message.LParam))))) +
                          '> y <' + UnicodeString(IntToStr(Integer(ShortInt(HIWORD(message.LParam))))) +
                          '>';
        WM_PRINT:
        begin
            case (message.LParam) of
                PRF_CHECKVISIBLE: lParamName := 'PRF_CHECKVISIBLE';
                PRF_CHILDREN:     lParamName := 'PRF_CHILDREN';
                PRF_CLIENT:       lParamName := 'PRF_CLIENT';
                PRF_ERASEBKGND:   lParamName := 'PRF_ERASEBKGND';
                PRF_NONCLIENT:    lParamName := 'PRF_NONCLIENT';
                PRF_OWNED:        lParamName := 'PRF_OWNED';
            else
                lParamName := UnicodeString(IntToHex(message.WParam, 8));
            end;
        end;

        WM_PRINTCLIENT:
        begin
            case (message.LParam) of
                PRF_CHECKVISIBLE: lParamName := 'PRF_CHECKVISIBLE';
                PRF_CHILDREN:     lParamName := 'PRF_CHILDREN';
                PRF_CLIENT:       lParamName := 'PRF_CLIENT';
                PRF_ERASEBKGND:   lParamName := 'PRF_ERASEBKGND';
                PRF_NONCLIENT:    lParamName := 'PRF_NONCLIENT';
                PRF_OWNED:        lParamName := 'PRF_OWNED';
            else
                lParamName := UnicodeString(IntToHex(message.WParam, 8));
            end;
        end;
    else
        lParamName := UnicodeString(IntToHex(message.WParam, 8));
    end;

    // format and return Windows message as text
    if (Assigned(pOwner)) then
        Result := 'Component - '  + UnicodeString(pOwner.Name) +
                  ' - message - ' + name        +
                  ' - wParam ['   + wParamName  +
                  '] - lParam ['  + lParamName  +
                  ']'
    else
        Result := 'Message - '   + name       +
                  ' - wParam ['  + wParamName +
                  '] - lParam [' + lParamName +
                  ']';
end;
//--------------------------------------------------------------------------------------------------

end.
