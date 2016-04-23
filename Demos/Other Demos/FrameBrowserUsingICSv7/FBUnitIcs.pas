{
  Version   11.6
  Copyright (c) 1995-2008 by L. David Baldwin
  Copyright (c) 2008-2010 by HtmlViewer Team
  Copyright (c) 2012-2015 by Angus Robertson delphi@magsys.co.uk
  Copyright (c) 2013-2015 by HtmlViewer Team

  Permission is hereby granted, free of charge, to any person obtaining a copy of
  this software and associated documentation files (the "Software"), to deal in
  the Software without restriction, including without limitation the rights to
  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
  of the Software, and to permit persons to whom the Software is furnished to do
  so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

  Note that the source modules HTMLGIF1.PAS and DITHERUNIT.PAS
  are covered by separate copyright notices located in those modules.

  ------------------------------------------------------------------------------------

  This demo requires the HtmlViewer component from:

  https://github.com/BerndGabriel/HtmlViewer

  which must also be downloaded and installed before the demo can be built.

  It also needs ICS v7 dated 1st April 2012 or later or ICS v8 from:

  http://wiki.overbyte.be/wiki/index.php/ICS_Download


  19 March 2012 - Angus Robertson replaced Indy with ICSv7 (based on FBUnitIndy)
  Added new diagnostic window showing all HTTP traffic and non-display HTML
  Save window sizes and positions
  Update UrlComboBox with each page visited
  New settings to stop caching pages (initially off) and images (on)
  Fixed bug with HotSpotTargetClick not always using a full URL
  Save a few more file types as binary downloads rather than displaying them

  21 March 2012 - fixed Unicode bugs with D2009 and later

  22 March 2012 - added new setting to log HTML page code for diagnostics

  23 March 2012 - added HTTP GZIP compression support in UrlConIcs

  10 April 2012 - Arno improved authentication but still need to cache URL/logins
                  Arno fixed multipart/form-data file upload
                  Arno reformatted all units with XE2 style formatter so clearer

  17 April 2012 - Angus - removed unnecessary timer to insert downloaded images
                  Now using a download image queue and a fixed number of static
                  image download sessions (MaxImageSess=4) rather than downloading
                  potentially dozens of images simultaneously in separate sessions.
                  This allows http/1.1 and keep-alive to work.  Likewise, Connection
                  used for html and css pages is no longer always closed.

  7 Oct 2012      Angus - recognise XE3 in About, supports ICS v7 and v8

  7  Oct 2013     Angus - simplfied Agent since Windows 98 long dead, recognise XE4 and XE5

  27 Nov 2013     Angus - renamed Proxy form to Settings, added User Agent so it's configerable

  7 Oct 2015      Angus - updated to work with 11.6 which has changed InsertImage, ImageObject
                  and onGetPostRequestEx, recognise XE6 to 10 Seattle in About


  Pending - use NoCache header to stop dynamic pages being cached and expire them
  Pending - cache visited links so we can highlight them


  Fixed bug in FramBrwz.pas with links not normalised
  Fixed bug in htmlview.pas with Referrer having application file path added to front
  Fixed bug in ReadHTML.pas handle meta without http-equiv="Content-Type" <meta charset="utf-8"> from HTML5
}

unit FBUnitIcs;

{$include htmlcons.inc}
{$WARN UNIT_PLATFORM OFF}
{ A program to demonstrate the TFrameBrowser component }

interface

uses
    WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
    ShellAPI, Menus, StdCtrls, Buttons, ExtCtrls, Gauges, mmSystem, IniFiles,
    MPlayer, ImgList, ComCtrls, ToolWin, Dialogs,
    htmlun2, CachUnitId, URLSubs, htmlview, htmlsubs, FramBrwz, FramView,
{$IFDEF UseOldPreviewForm}
  PreviewForm,
{$ELSE UseOldPreviewForm}
  BegaZoom,
  BegaHtmlPrintPreviewForm,
{$ENDIF UseOldPreviewForm}
    DownLoadId, Readhtml, urlconIcs,
    OverbyteIcsWndControl, OverbyteIcsWsocket, OverbyteIcsHttpProt,
    OverbyteIcsCookies, OverbyteIcsStreams, OverbyteIcsUtils,
    OverbyteIcsMimeUtils,
{$IF CompilerVersion >= 15}
{$IF CompilerVersion < 23}
    XpMan,
{$IFEND}
{$IFEND}
    HtmlGlobals, System.ImageList;

const
//    UsrAgent     = 'Mozilla/4.0 (compatible; MSIE 5.0; Windows 98)';
    UsrAgent     = 'Mozilla/4.0';
    MaxHistories = 15; { size of History list }
    MaxImageSess = 4;  { ANGUS how many simultaneous image downloads }
    wm_LoadURL   = wm_User + 124;
    wm_DownLoad  = wm_User + 125;
    WM_Next_Image = wm_User + 126;

    ImgStateFree = 1 ; ImgStateRequest = 2 ; ImgStateWaiting = 3 ; ImgStateCancel = 4 ;

type
    ImageRec = class(TObject)
        Viewer : ThtmlViewer;
        URL    : String;
    end;

    TImageHTTP = class(TComponent)
        { an HTTP component with a few extra fields }
    public
        ImRec      : ImageRec;
        URL        : String;
        Connection : TURLConnection;
        ImgState   : integer;     // ANGUS
        constructor CreateIt(AOwner : TComponent; IRec : TObject);
        destructor Destroy; override;
        procedure GetAsync;
    end;

    THTTPForm = class(TForm)
        MainMenu1 : TMainMenu;
        HistoryMenuItem : TMenuItem;
        Help1 : TMenuItem;
        File1 : TMenuItem;
        Openfile1 : TMenuItem;
        OpenDialog : TOpenDialog;
        Panel2 : TPanel;
        Status1 : TPanel;
        Status3 : TPanel;
        Status2 : TPanel;
        Options1 : TMenuItem;
        DeleteCache1 : TMenuItem;
        N1 : TMenuItem;
        Exit1 : TMenuItem;
        ShowImages : TMenuItem;
        SaveDialog : TSaveDialog;
        Edit1 : TMenuItem;
        Find1 : TMenuItem;
        Copy1 : TMenuItem;
        SelectAll1 : TMenuItem;
        N2 : TMenuItem;
        FindDialog : TFindDialog;
        PopupMenu : TPopupMenu;
        SaveImageAs : TMenuItem;
        N3 : TMenuItem;
        OpenInNewWindow : TMenuItem;
        FrameBrowser : TFrameBrowser;
        PrintPreview : TMenuItem;
        Print1 : TMenuItem;
        PrintDialog : TPrintDialog;
        Proxy1 : TMenuItem;
        DemoInformation1 : TMenuItem;
        About1 : TMenuItem;
        Timer1 : TTimer;
        CoolBar1 : TCoolBar;
        ToolBar2 : TToolBar;
        BackButton : TToolButton;
        FwdButton : TToolButton;
        ToolButton1 : TToolButton;
        ReloadButton : TToolButton;
        UrlComboBox : TComboBox;
        Panel10 : TPanel;
        ToolBar1 : TToolBar;
        CancelButton : TToolButton;
        SaveUrl : TToolButton;
        ImageList1 : TImageList;
        Panel3 : TPanel;
        Animate1 : TAnimate;
        Gauge : TProgressBar;
        SslContext : TSslContext;
        ShowDiagWindow : TMenuItem;
        IcsCookies : TIcsCookies;
        CachePages : TMenuItem;
        CacheImages : TMenuItem;
        ShowLogHTML : TMenuItem;
        MimeTypesList1 : TMimeTypesList;
        ShowLogHTTP: TMenuItem;
        procedure FormCreate(Sender : TObject);
        procedure FormDestroy(Sender : TObject);
        procedure GetButtonClick(Sender : TObject);
        procedure CancelButtonClick(Sender : TObject);
        procedure HistoryChange(Sender : TObject);
        procedure BackButtonClick(Sender : TObject);
        procedure FwdButtonClick(Sender : TObject);
        procedure Openfile1Click(Sender : TObject);
        procedure DeleteCacheClick(Sender : TObject);
        procedure Exit1Click(Sender : TObject);
        procedure ShowImagesClick(Sender : TObject);
        procedure ReloadClick(Sender : TObject);
        procedure FormShow(Sender : TObject);
        procedure Find1Click(Sender : TObject);
        procedure FindDialogFind(Sender : TObject);
        procedure Edit1Click(Sender : TObject);
        procedure SelectAll1Click(Sender : TObject);
        procedure Copy1Click(Sender : TObject);
        procedure URLComboBoxKeyPress(Sender : TObject; var Key : Char);
        procedure SaveImageAsClick(Sender : TObject);
        procedure URLComboBoxClick(Sender : TObject);
        procedure RightClick(Sender : TObject;
            Parameters : TRightClickParameters);
        procedure OpenInNewWindowClick(Sender : TObject);
        procedure SaveURLClick(Sender : TObject);
        procedure HTTPDocData1(Sender : TObject; Buffer : Pointer;
            Len : Integer);
        procedure FormClose(Sender : TObject; var Action : TCloseAction);
        procedure Status2Resize(Sender : TObject);
        procedure Processing(Sender : TObject; ProcessingOn : Boolean);
        procedure PrintPreviewClick(Sender : TObject);
        procedure File1Click(Sender : TObject);
        procedure Print1Click(Sender : TObject);
        procedure PrintHeader(Sender : TObject; Canvas : TCanvas;
            NumPage, W, H : Integer; var StopPrinting : Boolean);
        procedure PrintFooter(Sender : TObject; Canvas : TCanvas;
            NumPage, W, H : Integer; var StopPrinting : Boolean);
        procedure About1Click(Sender : TObject);
        procedure ViewerClear(Sender : TObject);
        procedure Proxy1Click(Sender : TObject);
        procedure DemoInformation1Click(Sender : TObject);
        procedure Timer1Timer(Sender : TObject);
        procedure FrameBrowserMouseMove(Sender : TObject; Shift : TShiftState;
            X, Y : Integer);
{$IFDEF UNICODE}
        procedure BlankWindowRequest(Sender : TObject;
            const Target, URL : String);
        procedure FrameBrowserGetPostRequestEx(Sender: TObject; IsGet: boolean;   // 11.6 changed slightly
            const URL, Query, EncType, Referer: String; Reload: boolean;
            var NewURL: String; var DocType: ThtmlFileType; var Stream: TStream);
        procedure GetImageRequest(Sender : TObject; const URL : String;
            var Stream : TStream);
        procedure HotSpotTargetClick(Sender : TObject;
            const Target, URL : String; var Handled : Boolean);
        procedure HotSpotTargetCovered(Sender : TObject;
            const Target, URL : String);
        procedure FrameBrowserScript(Sender : TObject;
            const Name, ContentType, Src, Script : String);
        procedure FrameBrowserMeta(Sender : TObject;
            const HttpEq, Name, Content : String);
        procedure FrameBrowserFileBrowse(Sender, Obj : TObject; var S : String);
        procedure FrameBrowserFormSubmit(Sender : TObject; Viewer : ThtmlViewer;
            const Action, Target, EncType, Method : String;
            Results : TStringList; var Handled : Boolean);
{$ELSE}
        procedure BlankWindowRequest(Sender : TObject;
            const Target, URL : WideString);
        procedure FrameBrowserGetPostRequestEx(Sender: TObject; IsGet: boolean;          // 11.6 changed slightly
            const URL, Query, EncType, Referer: WideString; Reload: boolean;
            var NewURL: WideString; var DocType: ThtmlFileType; var Stream: TStream);
        procedure GetImageRequest(Sender : TObject; const URL : WideString;
            var Stream : TStream);
        procedure HotSpotTargetClick(Sender : TObject;
            const Target, URL : WideString; var Handled : Boolean);
        procedure HotSpotTargetCovered(Sender : TObject;
            const Target, URL : WideString);
        procedure FrameBrowserScript(Sender : TObject;
            const Name, ContentType, Src, Script : WideString);
        procedure FrameBrowserMeta(Sender : TObject;
            const HttpEq, Name, Content : WideString);
        procedure FrameBrowserFileBrowse(Sender, Obj : TObject;
            var S : WideString);
        procedure FrameBrowserFormSubmit(Sender : TObject; Viewer : ThtmlViewer;
            const Action, Target, EncType, Method : WideString;
            Results : ThtStringList; var Handled : Boolean);
{$ENDIF}
        procedure ShowDiagWindowClick(Sender : TObject);
        procedure LogLine(S : String);
        procedure IcsCookiesNewCookie(Sender : TObject; ACookie : TCookie;
            var Save : Boolean);
        procedure CachePagesClick(Sender : TObject);
        procedure CacheImagesClick(Sender : TObject);
        procedure ShowLogHTMLClick(Sender : TObject);
    procedure ShowLogHTTPClick(Sender: TObject);
    private
        { Private declarations }
        URLBase     : String;
        Histories   : array [0 .. MaxHistories - 1] of TMenuItem;
        Pending     : TList; { a list of ImageRecs waiting for download }
        DiskCache   : TDiskCache;
        NewLocation : String;
        ARealm      : String;
        CurrentLocalFile, DownLoadUrl : String;
        Reloading                 : Boolean;
        FoundObject               : TImageObj;
        FoundObjectName           : String;
        NewWindowFile             : String;
        AnAbort                   : Boolean;
        NumImageTot, NumImageDone : Integer;
        AStream                   : TMemorystream;
        Connection                : TURLConnection;
        Proxy                     : String;
        ProxyPort                 : String;
        ProxyUser                 : String;
        ProxyPassword             : String;
        UserAgent                 : String;  { Angus } 
        TimerCount                : Integer;
        OldTitle                  : ThtString;
        HintWindow                : ThtHintWindow;
        HintVisible               : Boolean;
        TitleViewer               : ThtmlViewer;
        Allow                     : String;
        FCurRawSubmitValues       : ThtStringList;
        FCurSubmitCodepage        : LongWord;
        FImageHTTPs               : array of TImageHTTP;  // ANGUS, base1
        FCurHttpSess              : integer;
        LastProtocol              : String;

        procedure EnableControls;
        procedure DisableControls;
        procedure ImageRequestDone(Sender : TObject; RqType : THttpRequest;
            Error : Word);
        procedure HistoryClick(Sender : TObject);
        procedure WMLoadURL(var Message : TMessage); message wm_LoadURL;
        procedure WMDownLoad(var Message : TMessage); message wm_DownLoad;
        procedure WMNextImage(var AMsg : TMessage); message WM_Next_Image;
        procedure HttpSessionFree (session: integer);
        procedure HttpStopSessions;
        procedure CheckEnableControls;
        procedure ClearProcessing;
        procedure Progress(Num, Den : Integer);
        procedure wmDropFiles(var Message : TMessage); message wm_DropFiles;
        procedure CheckException(Sender : TObject; E : Exception);
        procedure HTTPRedirect(Sender : TObject);
        procedure HTTPSetCookie(Sender : TObject; const Data : String;
            var Accept : Boolean);
        procedure CloseHints;
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    public
        { Public declarations }
        FIniFilename : String;
    end;

    EContentTypeError = class(Exception);
    ESpecialException = class(Exception);

const
    CookieFile = 'Cookies.txt';

var
    HTTPForm : THTTPForm;
    Mon      : TextFile;
    Monitor1 : Boolean;
    Cache    : String;

implementation

uses
{$ifdef Compiler24_Plus}
  System.Types,
{$endif}
{$ifdef HasSystemUITypes}
  System.UITypes,
{$endif}
    HTMLAbt, ProxyDlg, AuthUnit, LogWin;

{$IFDEF LCL}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$IF CompilerVersion < 15}
{$R manifest.res}
{$IFEND}
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert a string in Windows character set to HTML texte. That is replace  }
{ all character with code between 160 and 255 by special sequences.         }
{ For example, 'fête' is replaced by 'f&ecirc;te'                           }
{ Also handle '<', '>', quote and double quote                              }
{ Replace multiple spaces by a single space followed by the required number }
{ of non-breaking-spaces (&nbsp;)                                           }
{ Replace TAB by a non-breaking-space.                                      }

function TextToHtmlText(const Src : ThtString) : String;
const
    HtmlSpecialChars : array [160..255] of String = (
        'nbsp'   , { #160 no-break space = non-breaking space               }
        'iexcl'  , { #161 inverted exclamation mark                         }
        'cent'   , { #162 cent sign                                         }
        'pound'  , { #163 pound sign                                        }
        'curren' , { #164 currency sign                                     }
        'yen'    , { #165 yen sign = yuan sign                              }
        'brvbar' , { #166 broken bar = broken vertical bar,                 }
        'sect'   , { #167 section sign                                      }
        'uml'    , { #168 diaeresis = spacing diaeresis                     }
        'copy'   , { #169 copyright sign                                    }
        'ordf'   , { #170 feminine ordinal indicator                        }
        'laquo'  , { #171 left-pointing double angle quotation mark         }
        'not'    , { #172 not sign                                          }
        'shy'    , { #173 soft hyphen = discretionary hyphen,               }
        'reg'    , { #174 registered sign = registered trade mark sign,     }
        'macr'   , { #175 macron = spacing macron = overline = APL overbar  }
        'deg'    , { #176 degree sign                                       }
        'plusmn' , { #177 plus-minus sign = plus-or-minus sign,             }
        'sup2'   , { #178 superscript two = superscript digit two = squared }
        'sup3'   , { #179 superscript three = superscript digit three = cubed }
        'acute'  , { #180 acute accent = spacing acute,                     }
        'micro'  , { #181 micro sign                                        }
        'para'   , { #182 pilcrow sign = paragraph sign,                    }
        'middot' , { #183 middle dot = Georgian comma = Greek middle dot    }
        'cedil'  , { #184 cedilla = spacing cedilla                         }
        'sup1'   , { #185 superscript one = superscript digit one           }
        'ordm'   , { #186 masculine ordinal indicator,                      }
        'raquo'  , { #187 right-pointing double angle quotation mark = right pointing guillemet }
        'frac14' , { #188 vulgar fraction one quarter = fraction one quarter}
        'frac12' , { #189 vulgar fraction one half = fraction one half      }
        'frac34' , { #190 vulgar fraction three quarters = fraction three quarters }
        'iquest' , { #191 inverted question mark = turned question mark     }
        'Agrave' , { #192 latin capital letter A with grave = latin capital letter A grave, }
        'Aacute' , { #193 latin capital letter A with acute,                }
        'Acirc'  , { #194 latin capital letter A with circumflex,           }
        'Atilde' , { #195 latin capital letter A with tilde,                }
        'Auml'   , { #196 latin capital letter A with diaeresis,            }
        'Aring'  , { #197 latin capital letter A with ring above = latin capital letter A ring, }
        'AElig'  , { #198 latin capital letter AE = latin capital ligature AE, }
        'Ccedil' , { #199 latin capital letter C with cedilla,              }
        'Egrave' , { #200 latin capital letter E with grave,                }
        'Eacute' , { #201 latin capital letter E with acute,                }
        'Ecirc'  , { #202 latin capital letter E with circumflex,           }
        'Euml'   , { #203 latin capital letter E with diaeresis,            }
        'Igrave' , { #204 latin capital letter I with grave,                }
        'Iacute' , { #205 latin capital letter I with acute,                }
        'Icirc'  , { #206 latin capital letter I with circumflex,           }
        'Iuml'   , { #207 latin capital letter I with diaeresis,            }
        'ETH'    , { #208 latin capital letter ETH                          }
        'Ntilde' , { #209 latin capital letter N with tilde,                }
        'Ograve' , { #210 latin capital letter O with grave,                }
        'Oacute' , { #211 latin capital letter O with acute,                }
        'Ocirc'  , { #212 latin capital letter O with circumflex,           }
        'Otilde' , { #213 latin capital letter O with tilde,                }
        'Ouml'   , { #214 latin capital letter O with diaeresis,            }
        'times'  , { #215 multiplication sign                               }
        'Oslash' , { #216 latin capital letter O with stroke = latin capital letter O slash, }
        'Ugrave' , { #217 latin capital letter U with grave,                }
        'Uacute' , { #218 latin capital letter U with acute,                }
        'Ucirc'  , { #219 latin capital letter U with circumflex,           }
        'Uuml'   , { #220 latin capital letter U with diaeresis,            }
        'Yacute' , { #221 latin capital letter Y with acute,                }
        'THORN'  , { #222 latin capital letter THORN,                       }
        'szlig'  , { #223 latin small letter sharp s = ess-zed,             }
        'agrave' , { #224 latin small letter a with grave = latin small letter a grave, }
        'aacute' , { #225 latin small letter a with acute,                  }
        'acirc'  , { #226 latin small letter a with circumflex,             }
        'atilde' , { #227 latin small letter a with tilde,                  }
        'auml'   , { #228 latin small letter a with diaeresis,              }
        'aring'  , { #229 latin small letter a with ring above = latin small letter a ring, }
        'aelig'  , { #230 latin small letter ae = latin small ligature ae   }
        'ccedil' , { #231 latin small letter c with cedilla,                }
        'egrave' , { #232 latin small letter e with grave,                  }
        'eacute' , { #233 latin small letter e with acute,                  }
        'ecirc'  , { #234 latin small letter e with circumflex,             }
        'euml'   , { #235 latin small letter e with diaeresis,              }
        'igrave' , { #236 latin small letter i with grave,                  }
        'iacute' , { #237 latin small letter i with acute,                  }
        'icirc'  , { #238 latin small letter i with circumflex,             }
        'iuml'   , { #239 latin small letter i with diaeresis,              }
        'eth'    , { #240 latin small letter eth                            }
        'ntilde' , { #241 latin small letter n with tilde,                  }
        'ograve' , { #242 latin small letter o with grave,                  }
        'oacute' , { #243 latin small letter o with acute,                  }
        'ocirc'  , { #244 latin small letter o with circumflex,             }
        'otilde' , { #245 latin small letter o with tilde,                  }
        'ouml'   , { #246 latin small letter o with diaeresis,              }
        'divide' , { #247 division sign                                     }
        'oslash' , { #248 latin small letter o with stroke, = latin small letter o slash, }
        'ugrave' , { #249 latin small letter u with grave,                  }
        'uacute' , { #250 latin small letter u with acute,                  }
        'ucirc'  , { #251 latin small letter u with circumflex,             }
        'uuml'   , { #252 latin small letter u with diaeresis,              }
        'yacute' , { #253 latin small letter y with acute,                  }
        'thorn'  , { #254 latin small letter thorn,                         }
        'yuml');   { #255 latin small letter y with diaeresis,              }
var
    I, J : Integer;
    Sub  : String;
begin
    Result := '';
    I := 1;
    while I <= Length(Src) do begin
        J   := I;
        Sub := '';
        while (I <= Length(Src)) and (Ord(Src[I]) < Low(HtmlSpecialChars)) do begin
            case Src[I] of
            ' '  : begin
                       if (I > 1) and (Src[I - 1] = ' ') then begin
                           { Replace multiple spaces by &nbsp; }
                           while (I <= Length(Src)) and (Src[I] = ' ') do begin
                               Sub := Sub + '&nbsp;';
                               Inc(I);
                           end;
                           Dec(I);
                       end
                       else
                           Inc(I);
                   end;
            '<'  : Sub := '&lt;';
            '>'  : Sub := '&gt;';
            '''' : sub := '&#39;';
            '"'  : Sub := '&#34;';
            '&'  : Sub := '&amp;';
            #9   : Sub := '&nbsp;';
            #10  : Sub := #10'<BR>';
            else
                Inc(I);
            end;
            if Length(Sub) > 0 then begin
                Result := Result + Copy(Src, J, I - J) + Sub;
                Inc(I);
                J      := I;
                Sub    := '';
            end;
        end;

        if I > Length(Src) then begin
            Result := Result + Copy(Src, J, I - J);
            Exit;
        end;
        if Ord(Src[I]) > 255 then
            Result := Result + Copy(Src, J, I - J) + '&#' + IntToStr(Ord(Src[I])) + ';'
        else
            Result := Result + Copy(Src, J, I - J) + '&' +
                    HtmlSpecialChars[Ord(Src[I])] + ';';
        Inc(I);
    end;
end;


procedure StartProcess(CommandLine : String; ShowWindow : Word);
var
    si : _STARTUPINFO;
    pi : _PROCESS_INFORMATION;
begin
    FillChar(si, SizeOf(si), 0);
    si.cb          := SizeOf(si);
    si.dwFlags     := STARTF_USESHOWWINDOW;
    si.wShowWindow := ShowWindow;
    UniqueString(CommandLine);
    if CreateProcess(nil, PChar(CommandLine), nil, nil, False, 0, nil, nil, si,
        pi) then begin
        CloseHandle(pi.hProcess);
        CloseHandle(pi.hThread);
    end;
end;

{ ----------------THTTPForm.FormCreate }
procedure THTTPForm.FormCreate(Sender : TObject);
var
    I, J    : Integer;
    IniFile : TIniFile;
    SL      : TStringList;
    S       : String;
begin
{$IFDEF HasGestures}
  FrameBrowser.Touch.InteractiveGestureOptions := [igoPanSingleFingerHorizontal,
    igoPanSingleFingerVertical, igoPanInertia];
  FrameBrowser.Touch.InteractiveGestures := [igPan];
{$ENDIF}
    Top := Top div 2;
    if Screen.Width <= 800 then { make window fit appropriately }
    begin
        Left   := Left div 2;
        Width  := (Screen.Width * 9) div 10;
        Height := (Screen.Height * 7) div 8;
    end
    else begin
        Width  := 850;
        Height := 600;
    end;

    Cache           := ExtractFilePath(Application.ExeName) + 'Cache\';
    DiskCache       := TDiskCache.Create(Cache);
    Status1.Caption := '';

    { Monitor1 will be set if this is the first instance opened }
    Monitor1 := True;
    try
        AssignFile(Mon, Cache + 'Monitor.txt');
        { a monitor file for optional use }
        Rewrite(Mon);
    except
        Monitor1 := False; { probably open in another instance }
    end;

{$IFDEF LogIt}
    if Monitor1 then begin
        DeleteFile(Cache + 'LogFile.txt');
        Log             := TModIdLogFile.Create(Self);
        Log.Filename    := Cache + 'LogFile.txt';
        Log.ReplaceCRLF := False;
        Log.Active      := True;
    end;
{$ENDIF}
    IcsCookies.LoadFromFile(Cache + CookieFile);
    IcsCookies.AutoSave := True;
    AStream             := TMemorystream.Create;
    Pending             := TList.Create;

 { ANGUS create several image session components, base1 }
    SetLength (FImageHTTPs, MaxImageSess + 1);
    for I := 1 to MaxImageSess do begin
        FImageHTTPs [I] := TImageHTTP.CreateIt(Self, Nil);
        with FImageHTTPs [I] do begin
            ImgState                 := ImgStateFree;
            Connection.Session       := I;  // body is 0
            Connection.Owner         := FImageHTTPs [I];
            Connection.OnCookie      := HTTPForm.HTTPSetCookie;
            Connection.OnRequestDone := ImageRequestDone;
        end;
    end;

    FrameBrowser.HistoryMaxCount := MaxHistories;
    { defines size of history list }

    for I := 0 to MaxHistories - 1 do
    begin { create the MenuItems for the history list }
        Histories[I] := TMenuItem.Create(HistoryMenuItem);
        HistoryMenuItem.Insert(I, Histories[I]);
        with Histories[I] do begin
            Visible := False;
            OnClick := HistoryClick;
            Tag     := I;
        end;
    end;

    { Load animation from resource }
    Animate1.ResName := 'StarCross';

{$IFDEF ver140}   { delphi 6 }
    UrlComboBox.AutoComplete := False;
{$ENDIF}
    I            := Pos('.', Application.ExeName);
    FIniFilename := Copy(Application.ExeName, 1, I) + 'ini';

    ProxyPort := '80';
    IniFile   := TIniFile.Create(FIniFilename);
    try
        Top    := IniFile.ReadInteger('HTTPForm', 'Top', Top);
        Left   := IniFile.ReadInteger('HTTPForm', 'Left', Left);
        Width  := IniFile.ReadInteger('HTTPForm', 'Width', Width);
        Height := IniFile.ReadInteger('HTTPForm', 'Height', Height);
        ShowDiagWindow.Checked := IniFile.ReadBool('HTTPForm', 'ShowDiagWindow',
            ShowDiagWindow.Checked);
        CachePages.Checked := IniFile.ReadBool('HTTPForm', 'CachePages',
            CachePages.Checked);
        CacheImages.Checked := IniFile.ReadBool('HTTPForm', 'CacheImages',
            CacheImages.Checked);
        ShowLogHTML.Checked := IniFile.ReadBool('HTTPForm', 'ShowLogHTML',
            ShowLogHTML.Checked);
        ShowLogHTTP.Checked := IniFile.ReadBool('HTTPForm', 'ShowLogHTTP',
            ShowLogHTTP.Checked);
        Proxy         := IniFile.ReadString('Proxy', 'ProxyHost', '');
        ProxyPort     := IniFile.ReadString('Proxy', 'ProxyPort', '80');
        ProxyUser     := IniFile.ReadString('Proxy', 'ProxyUsername', '');
        ProxyPassword := IniFile.ReadString('Proxy', 'ProxyPassword', '');
        UserAgent     := IniFile.ReadString('Settings', 'UserAgent', UsrAgent);
        SL            := TStringList.Create;
        try
            IniFile.ReadSectionValues('favorites', SL);
            for I := 0 to SL.Count - 1 do begin
                J := Pos('=', SL[I]);
                S := Copy(SL[I], J + 1, 1000);
                if UrlComboBox.Items.IndexOf(S) < 0 then
                    UrlComboBox.Items.Add(S);
            end;
        finally
            SL.Free;
        end;
    finally
        IniFile.Free;
    end;
    DragAcceptFiles(Handle, True);
    Application.OnException := CheckException;

{$IFDEF M4Viewer}
    ProtocolHandler := M4ProtocolHandler;
{$ENDIF}
    HintWindow       := ThtHintWindow.Create(Self);
    HintWindow.Color := $C0FFFF;

  Application.OnMessage := AppMessage;
end;

{ ----------------THTTPForm.FormDestroy }
procedure THTTPForm.FormDestroy(Sender : TObject);
var
    I       : Integer;
    IniFile : TIniFile;
begin
    if Monitor1 then begin { save only if this is the first instance }
        IniFile := TIniFile.Create(FIniFilename);
        try
            IniFile.WriteInteger('HTTPForm', 'Top', Top);
            IniFile.WriteInteger('HTTPForm', 'Left', Left);
            IniFile.WriteInteger('HTTPForm', 'Width', Width);
            IniFile.WriteInteger('HTTPForm', 'Height', Height);
            IniFile.WriteBool('HTTPForm', 'ShowDiagWindow',
                ShowDiagWindow.Checked);
            IniFile.WriteBool('HTTPForm', 'CachePages', CachePages.Checked);
            IniFile.WriteBool('HTTPForm', 'CacheImages', CacheImages.Checked);
            IniFile.WriteBool('HTTPForm', 'ShowLogHTML', ShowLogHTML.Checked);
            IniFile.WriteBool('HTTPForm', 'ShowLogHTTP', ShowLogHTTP.Checked);
            IniFile.WriteString('Proxy', 'ProxyHost', Proxy);
            IniFile.WriteString('Proxy', 'ProxyPort', ProxyPort);
            IniFile.WriteString('Proxy', 'ProxyUsername', ProxyUser);
            IniFile.WriteString('Proxy', 'ProxyPassword', ProxyPassword);
            IniFile.WriteString('Settings', 'UserAgent', UserAgent);
            IniFile.EraseSection('Favorites');
            for I := 0 to UrlComboBox.Items.Count - 1 do
                IniFile.WriteString('Favorites', 'Url' + IntToStr(I),
                    UrlComboBox.Items[I]);
        finally
            IniFile.Free;
        end;
    end;
    AStream.Free;
    Pending.Free;
    try
        if FCurHttpSess > 0 then
            HttpStopSessions;
        for I := 1 to MaxImageSess do begin
            if Assigned (FImageHTTPs [I]) then begin
                FImageHTTPs [I].Free;
                FImageHTTPs [I] := Nil;
            end;
        end;
    except
    end;
    if Monitor1 then
        CloseFile(Mon);
    DiskCache.Free;
    FCurRawSubmitValues.Free;
    if Assigned(Connection) then begin
        Connection.Free;
        Connection := nil;
    end;
end;

procedure THTTPForm.LogLine(S : String);
begin
    if not ShowDiagWindow.Checked then
        exit;
    if not LogForm.Visible then
        LogForm.Visible := True;
    LogForm.LogMemo.Lines.Add(S);
end;

{ ----------------THTTPForm.GetButtonClick }
procedure THTTPForm.GetButtonClick(Sender : TObject);
{ initiate loading of a main document }
begin
    DisableControls;
    Status1.Caption := '';
    Status2.Caption := '';
    try
        { the following initiates one or more GetPostRequest's }
        UrlComboBox.Text := Normalize(UrlComboBox.Text);
        URLBase          := GetUrlBase(UrlComboBox.Text);
        FrameBrowser.LoadURL(UrlComboBox.Text);
        Reloading := False;
    finally
        CheckEnableControls;
        FrameBrowser.SetFocus;
    end;
end;

procedure THTTPForm.FrameBrowserFileBrowse(Sender, Obj : TObject;
    var S : ThtString);
begin
    with TOpenDialog.Create(nil) do
        try
            Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist];
            if Execute then
                S := Filename;
        finally
            Free;
        end;
end;

procedure THTTPForm.FrameBrowserFormSubmit(Sender : TObject;
    Viewer : ThtmlViewer; const Action, Target, EncType, Method : ThtString;
    Results : ThtStringList; var Handled : Boolean);
begin
    if FCurRawSubmitValues = nil then
        FCurRawSubmitValues := ThtStringList.Create;
    FCurRawSubmitValues.Assign(Results);
    FCurSubmitCodepage := Viewer.CodePage;
end;

{ ----------------THTTPForm.FrameBrowserGetPostRequestEx }
procedure THTTPForm.FrameBrowserGetPostRequestEx(Sender: TObject; IsGet: boolean;      // 11.6 changed slightly
    const URL, Query, EncType, Referer: ThtString; Reload: boolean;
    var NewURL: ThtString; var DocType: ThtmlFileType; var Stream: TStream);
{ OnGetPostRequest handler.
  URL is what to load.
  IsGet is set for Get (as opposed to Post)
  Query a possible query String
  Reload set if don't want what's in cache
  NewURL return in the document location has changed (happens quite frequently).
  DocType is the type of document found -- HTMLType, TextType, or ImgType
  Stream is the stream answer to the request }
const
    MaxRedirect = 15;
var
    S, URL1, FName, Query1, LastUrl : String;
    AnsiQuery                       : AnsiString;
    Error, TryAgain, TryRealm       : Boolean;
    RedirectCount                   : Integer;

    function GetAuthorization : Boolean;
    var
        UName, PWord : String;
    begin
        with AuthForm do begin
            Result := GetAuthorization(TryRealm and (ARealm <> ''), ARealm,
                UName, PWord);
            TryRealm := False; { only try it once }
            if Result then begin
                Connection.UserName := UName;
                Connection.Password := PWord;
                Connection.InputStream.Clear;
                { delete any previous message sent }
            end;
        end;
    end;

var
    Header, Footer, RandBoundary, Filename, InputName, InputValue : ThtString;
    protocol : String;
    I : Integer;

begin
    CloseHints; { may be a hint window open }
    Query1          := Query;
    Status1.Caption := '';
    Status2.Caption := '';
    FName           := '';
    Error           := False;
    AnAbort         := False;
    NumImageTot     := 0;
    NumImageDone    := 0;
    Progress(0, 0);
    Gauge.Visible := True;
    URL1          := Normalize(URL);
    URLBase       := GetUrlBase(URL1);
    DisableControls;
    NewLocation := '';
    { will change if document referenced by URL has been relocated }
    ARealm   := '';
    TryRealm := True;
    AStream.Clear;
    RedirectCount := 0;
    if Reloading or Reload or (not CachePages.Checked) then
    begin { don't want a cache file }
        DiskCache.RemoveFromCache(URL1);
    end
    else
        DiskCache.GetCacheFilename(URL1, FName, DocType, NewLocation);
    { if in cache already, get the cache filename }
    if (FName = '') or not FileExists(FName) then begin { it's not in cache }
        protocol := GetProtocol(URL1);
        if (protocol <> LastProtocol) and (Connection <> nil) then begin  { ANGUS don't stop http/1.1 working  }
            Connection.Free;
            Connection := nil;
        end;
        LastProtocol := protocol;  // ANGUS
        if Pos ('https://', LowerCase(URL1)) = 1 then
        try
            SslContext.InitContext;  { ANGUS check for SSL before making request }
        except
            on E:Exception do begin
                LogLine('Failed to initialize SSL Context: ' + E.Message);
                raise;
            end;
        end;
        if Connection = nil then
            Connection := TURLConnection.GetConnection(URL1);
        if Connection <> nil then begin
            Connection.Session       := 0;
            Connection.OnDocData     := HTTPForm.HTTPDocData1; // progress only
            Connection.Referer       := Referer;
            LastUrl                  := URL1;
            Connection.OnRedirect    := HTTPForm.HTTPRedirect;
            Connection.OnCookie      := HTTPForm.HTTPSetCookie;
            Connection.Proxy         := Proxy;
            Connection.ProxyPort     := ProxyPort;
            Connection.ProxyUser     := ProxyUser;
            Connection.ProxyPassword := ProxyPassword;
            Connection.UserAgent     := UserAgent;
            Connection.Cookie        := IcsCookies.GetCookies(URL1);
            try
                repeat
                    TryAgain := False;
                    Inc(RedirectCount);
                    if Assigned(Connection.InputStream) then
                        Connection.InputStream.Clear;
                    try
                        if IsGet then begin { Get }
                            if Query1 <> '' then begin
                                LogLine('FrameBrowser Get: ' + URL1 + '?' + Query1);
                                Connection.Get(URL1 + '?' + Query1);
                            end
                            else begin
                                LogLine('FrameBrowser Get: ' + URL1);
                                Connection.Get(URL1);
                            end;
                        end
                        else begin { Post }
                            if EncType = 'multipart/form-data' then { AG }
                            begin
                                Filename     := '';
                                RandBoundary := IntToHex(Random(MaxInt), 8);
                                Connection.ContentTypePost := EncType +
                                    '; boundary=---------------------------' +
                                    RandBoundary;
                                for I := 0 to FCurRawSubmitValues.Count - 1 do
                                begin
                                    if LowerCase(FCurRawSubmitValues.Names[I]) = 'file'
                                    then // no input type available :(
                                    begin
                                        { It's a file upload }
                                        Filename :=
                                        FCurRawSubmitValues.ValueFromIndex[I];
                                        InputValue := ExtractFileName(Filename);
                                        if not CheckUnicodeToAnsi(InputValue, FCurSubmitCodepage) then
                                          InputValue := TextToHtmlText(ExtractFileName(Filename)); // as FireFox
                                        Header := '-----------------------------'
                                            + RandBoundary + CRLF +
                                            'Content-Disposition: form-data; name="file"; filename="'
                                            + InputValue + '"' + CRLF +
                                            'Content-Type: ' +
                                            MimeTypesList1.TypeFromFile(InputValue)
                                            + CRLF + CRLF;
                                    end
                                    else begin
                                        InputName := FCurRawSubmitValues.Names[I];
                                        InputValue := FCurRawSubmitValues.ValueFromIndex[I];
                                        if not CheckUnicodeToAnsi(InputName, FCurSubmitCodepage) then
                                            InputName := TextToHtmlText (FCurRawSubmitValues.Names[I]);
                                        if not CheckUnicodeToAnsi(InputValue, FCurSubmitCodepage) then
                                            InputValue := TextToHtmlText (FCurRawSubmitValues.ValueFromIndex[I]);
                                        Footer := Footer + '-----------------------------' +
                                            RandBoundary + CRLF +
                                            'Content-Disposition: form-data; name="'
                                            + InputName + '"' + CRLF + CRLF +
                                            InputValue + CRLF;
                                    end;
                                end;

                                if Header <> '' then
                                    Footer := CRLF + Footer;
                                Footer := Footer + '-----------------------------' +
                                    RandBoundary + '--' + CRLF;
                                if Filename <> '' then // if it's a file upload
                                begin
                                    try
                                        Connection.SendStream := TMultiPartFileReader.Create(Filename,
                                                         UnicodeToAnsi(Header, FCurSubmitCodepage, True),
                                                         UnicodeToAnsi(Footer, FCurSubmitCodepage, True));
                                    except
                                        Connection.SendStream := TMemorystream.Create;
                                        raise;
                                    end;
                                end
                                else
                                    Connection.SendStream := TMemorystream.Create;
                                try
                                    LogLine('FrameBrowser Post: ' + URL1 + ', EncType=' + EncType);
                                    if Filename = '' then
                                    // No file upload, write to stream
                                    begin
                                        AnsiQuery := UnicodeToAnsi(Footer, FCurSubmitCodepage);
                                        Connection.SendStream.WriteBuffer (Pointer(AnsiQuery)^,
                                                                                Length(AnsiQuery));
                                    end;
                                    Connection.SendStream.Position := 0;
                                    Connection.Post(URL1);
                                finally
                                    Connection.SendStream.Free;
                                end;
                            end // EncType = 'multipart/form-data'
                            else begin
                                Connection.SendStream := TMemorystream.Create;
                                try
                                    LogLine('FrameBrowser Post: ' + URL1 +
                                        ', Data=' + Copy(Query1, 1, 1024) +
                                        ', EncType=' + EncType);
                                    // not too much data

                                    AnsiQuery := AnsiString(Query1);
                                    if EncType = '' then
                                        Connection.ContentTypePost := 'application/x-www-form-urlencoded'
                                    else
                                        Connection.ContentTypePost := EncType;
                                    Connection.SendStream.WriteBuffer (AnsiQuery[1], Length(AnsiQuery));
                                    Connection.SendStream.Position := 0;
                                    Connection.Post(URL1);
                                finally
                                    Connection.SendStream.Free;
                                end;
                            end;
                        end;
                        if Connection.StatusCode = 401 then begin
                            ARealm   := Connection.Realm;
                            TryAgain := GetAuthorization;
                        end;
                    except
                        case Connection.StatusCode of
                            401 : begin
                                    TryAgain := GetAuthorization;
                                end;
                            405 :
                                if not IsGet and (Pos('get', Allow) > 0) then
                                begin
                                    IsGet    := True;
                                    TryAgain := True;
                                end;
                            301, 302 :
                                if NewLocation <> '' then begin
                                    URL1     := NewLocation;
                                    TryAgain := True;
                                end;
                        end;
                        if not TryAgain or (RedirectCount >= MaxRedirect) then
                            raise;
                    end;
                    LogLine('FrameBrowserGetPostRequestEx Done: Status ' +
                        IntToStr(Connection.StatusCode));
                until not TryAgain;

                DocType := Connection.ContentType;
                AStream.LoadFromStream(Connection.InputStream);
            except
                on ESpecialException do begin
                    LogLine('FrameBrowserGetPostRequestEx: Special Exception');
                    Connection.Free; { needs to be reset }
                    Connection := nil;
                    raise;
                end;
                on E : Exception do
                    try
                        LogLine('FrameBrowserGetPostRequestEx: Exception - ' + E.Message);
                        if AnAbort then
                            raise (ESpecialException.Create ('Abort on user request'));

                        Error := True;
                        if Connection is THTTPConnection then
                            with Connection do begin
                                if InputStream.Size > 0
                                then { sometimes error messages come in RcvdStream }
                                    AStream.LoadFromStream(InputStream)
                                else begin
                                    if RedirectCount >= MaxRedirect then
                                        S := 'Excessive Redirects'
                                    else
                                        S := ReasonPhrase + '<p>Statuscode: ' +
                                        IntToStr(StatusCode);
                                    LogLine(S);
                                    AStream.Write(S[1], Length(S));
                                end;
                                DocType := HTMLType;
                            end
                        else { other connection types }
                        begin
                            S := E.Message; { Delphi 1 }
                            AStream.Write(S[1], Length(S));
                            LogLine(S);
                        end;
                    finally
                        Connection.Free; { needs to be reset }
                        Connection := nil;
                    end;
            end;
            Status1.Caption := 'Received ' + IntToStr(AStream.Size) + ' bytes';
            if CachePages.Checked then begin
                FName := DiskCache.AddNameToCache(LastUrl, NewLocation,
                    DocType, Error);
                if FName <> '' then
                    try
                        AStream.SaveToFile(FName); { it's now in cache }
                    except
                    end;
            end;
        end
        else begin { unsupported protocol }
            S := 'Unsupported protocol: ' + URL1; // GetProtocol(URL1);
            // if Sender is TFrameBrowser then   {main document display}
            // Raise(ESpecialException.Create(S))
            // else   {else other errors will get displayed as HTML file}
            AStream.Write(S[1], Length(S));
            LogLine(S);
        end;
    end
    else begin
        AStream.LoadFromFile(FName);
        LogLine('FrameBrowserGetPostRequestEx: from Cache' + FName);
    end;
    NewURL := NewLocation; { in case location has been changed }
    if ShowLogHTML.Checked then begin
        AStream.Position := 0;
        SetLength(AnsiQuery, AStream.Size);
        AStream.ReadBuffer(AnsiQuery[1], AStream.Size);
        LogLine(String(AnsiQuery));
    end;
    Stream := AStream;
end;

procedure THTTPForm.HTTPRedirect(Sender : TObject);
var
    Proto, Dest : String;
begin
    Dest := (Sender as TSslHttpCli).Location;
    LogLine('Redirected to: ' + Dest);
    Proto := GetProtocol(URLBase);
    if IsFullUrl(Dest) then { it's a full URL }
    begin
        NewLocation := Normalize(Dest);
    end
    else begin
        NewLocation := CombineURL(URLBase, Dest);
    end;
    URLBase := GetUrlBase(NewLocation);

    { cookies may have been sent during redirection, so update again now   }
    (Sender as TSslHttpCli).Cookie := IcsCookies.GetCookies(NewLocation);
end;

procedure THTTPForm.HTTPSetCookie(Sender : TObject; const Data : String;
    var Accept : Boolean);
begin
    IcsCookies.SetCookie(Data, (Sender as TSslHttpCli).URL);
end;

procedure THTTPForm.IcsCookiesNewCookie(Sender : TObject; ACookie : TCookie;
    var Save : Boolean);
var
    S : String;
begin
    with ACookie do begin
        S := 'NewCookie: ' + CName + '=' + CValue + ', Domain=' + CDomain + ', Path=' + CPath;
        if CPersist then
            S := S + ', Expires=' + DateTimeToStr(CExpireDT)
        else
            S := S + ', Not Persisent';
        if CSecureOnly then
            S := S + ', SecureOnly';
        if CHttpOnly then
            S := S + ', HttpOnly';
        LogLine(S);
    end;
end;

{ ----------------THTTPForm.GetImageRequest }
procedure THTTPForm.GetImageRequest(Sender : TObject; const URL : ThtString;
    var Stream : TStream);
{ the OnImageRequest handler }
var
    S       : String;
    IR      : ImageRec;
    DocType : ThtmlFileType;
    Dummy   : String;
begin
    if Reloading or (not CacheImages.Checked) then begin
        DiskCache.RemoveFromCache(URL);
        S := '';
    end
    else
        DiskCache.GetCacheFilename(URL, S, DocType, Dummy);
    { see if image is in cache file already }
    if FileExists(S) and (DocType = ImgType) then begin { yes, it is }
        AStream.LoadFromFile(S);
        Stream := AStream; { return image immediately }
        LogLine('GetImageRequest, from Cache: ' + S);
    end
  { see if res, file or zip protocols, get image immediately without a queue }
    else if not((GetProtocol(URL) = 'http') or (GetProtocol(URL) = 'https'))
    then begin
        if Assigned(Connection) then
            Connection.Free;
        Connection := TURLConnection.GetConnection(URL);
        if Connection <> nil then begin
            LogLine('GetImageRequest Start: ' + URL);
            try
                Connection.Get(URL);
                Stream := Connection.InputStream;
            except
                Stream := nil;
            end;
        end
        else
            Stream := nil;
    end
    else begin { http protocol, the image will need to be downloaded }
        Stream    := WaitStream; { wait indicator }
        IR        := ImageRec.Create;
        IR.Viewer := Sender as ThtmlViewer;
        IR.URL    := URL;

    { add image to queue, then see if any spare sessions to download it }
        Pending.Add(IR);
        Inc(NumImageTot);
        Progress(NumImageDone, NumImageTot);
        LogLine('GetImageRequest Queued: ' + URL);
        PostMessage(Handle, WM_Next_Image, 0, 0);
    end;
end;

{ ----------------THTTPForm.WMNextImage }

procedure THTTPForm.WMNextImage(var AMsg : TMessage);
var
    I, session: integer ;
    CurImage: ImageRec;
begin
    if Pending.Count = 0 then begin
        LogLine ('No more queued image downloads');
        Progress(NumImageDone, NumImageTot);
        exit;
    end;
    session := -1;
    for I := 1 to MaxImageSess do begin
        if FImageHTTPs [I].ImgState <= ImgStateFree then
        begin
            session := I;
            break;
        end;
    end;
    if session < 0 then begin { no free sessions for image downloads, will try later }
        exit;
    end;

{ start a download for first waiting URL, remove from list  }
    CurImage := Pending [0];
    Pending.Delete (0);
    with FImageHTTPs [session] do begin
        Connection.CheckInputStream;
        ImgState                 := ImgStateWaiting;
        ImRec                    := CurImage;
        URL                      := CurImage.URL;
        Connection.Proxy         := Proxy;
        Connection.ProxyPort     := ProxyPort;
        Connection.ProxyUser     := ProxyUser;
        Connection.ProxyPassword := ProxyPassword;
        Connection.UserAgent     := UserAgent;
        Connection.Cookie        := IcsCookies.GetCookies(URL);
        inc (FCurHttpSess);
        DisableControls;
        LogLine('[' + IntToStr (Connection.Session) + '] GetImageRequest Start: ' + URL);
        try
            GetAsync;
        except
            LogLine ('Exception starting image download: ' + URL);
            Inc(NumImageDone);
            ImgState := ImgStateFree;
        end;
    end;
end;

{ ----------------THTTPForm.HttpSessionFree }

procedure THTTPForm.HttpSessionFree (session: integer);
begin
    try
        if session <= 0 then exit ;
        Progress(NumImageDone, NumImageTot);
        if NOT Assigned (FImageHTTPs [session]) then exit;
        if (FImageHTTPs [session].ImgState > ImgStateFree) and (FCurHttpSess > 0) then
             dec (FCurHttpSess) ;
        FImageHTTPs [session].ImgState := ImgStateFree;
        FImageHTTPs [session].Connection.InputStream.Clear;
        if Pending.Count <> 0 then
            PostMessage(Handle, WM_Next_Image, 0, 0)   { next queued request  }
        else begin
            CheckEnableControls;
        end;
    except
        LogLine  ('[' + IntToStr (session) + '] !! Failed to Free Image Download Session');
        FImageHTTPs [session].ImgState := ImgStateFree;
    end;
end;

{ ----------------THTTPForm.HttpStopSessions }

procedure THTTPForm.HttpStopSessions;
var
    I: integer ;
begin
    Pending.Clear ;
    FCurHttpSess := 0;
    for I := 1 to MaxImageSess do
    begin
        if Assigned (FImageHTTPs [I]) then begin
            if FImageHTTPs [I].ImgState > ImgStateFree then begin
                FImageHTTPs [I].ImgState := ImgStateFree;
                try
                    if Assigned (FImageHTTPs [I].Connection) then begin
                        if Assigned (FImageHTTPs [I].Connection) then begin
                            LogLine  ('[' + IntToStr (I) + '] !! Aborting Image Download');
                            FImageHTTPs [I].ImgState := ImgStateCancel;
                            FImageHTTPs [I].Connection.Abort;   { sync }
                            FImageHTTPs [I].ImgState := ImgStateFree;
                        end;
                    end;
               except
                    ;
                end;
            end;
        end;
    end;
end;

{ ----------------THTTPForm.ImageRequestDone }

procedure THTTPForm.ImageRequestDone(Sender : TObject; RqType : THttpRequest;
    Error : Word);
{ arrive here when ImHTTP.GetAsync has completed }
var
    ImageHTTP: TImageHTTP;
    FName : String;
begin
    ImageHTTP := (Sender as TImageHTTP);
    try
        with ImageHTTP do begin
            if ImgState in [ImgStateFree, ImgStateCancel] then begin
                LogLine('[' + IntToStr (Connection.Session) + '] GetImageRequest Ignored: ' + URL);
            end
            else if (Error = 0) then begin
                LogLine('[' + IntToStr (Connection.Session) + '] GetImageRequest Done OK: ' + URL);
                if Assigned(Connection.InputStream) then begin { Stream can be Nil }
                    ImRec.Viewer.InsertImage (URL, Connection.InputStream) ;   // 11.6 was FrameBrowser.InsertImage but gone
                    { save image in cache file }
                    if CacheImages.Checked then begin
                        FName := DiskCache.AddNameToCache(URL, '', ImgType, False);
                        if FName <> '' then
                        try
                            ImageHTTP.Connection.InputStream.SaveToFile(FName);
                        except
                        end;
                    end;
                    Connection.InputStream.Clear;
                end
                else
                    LogLine('[' + IntToStr (Connection.Session) + '] GetImageRequest No Stream: ' + URL);
            end
            else
                LogLine('[' + IntToStr (Connection.Session) + '] GetImageRequest Failed, Error ' +
                                                           IntToStr (Error) + ': ' + URL);
        end;
    except
        LogLine('[' + IntToStr (Connection.Session) + '] GetImageRequest Exception: ' + ImageHTTP.URL);
    end;
    Inc(NumImageDone);
  { free session, also checks for more downloads, if any }
    HttpSessionFree (ImageHTTP.Connection.Session);
end;

procedure THTTPForm.CheckEnableControls;
begin
    if not FrameBrowser.Processing and (not Assigned(Connection) or
                 (Connection.State in [httpReady, httpNotConnected])) and
                         (Pending.Count = 0) and (FCurHttpSess = 0) then begin
        EnableControls;
        Status2.Caption := 'DONE';
    end;
end;

procedure THTTPForm.ClearProcessing;
var
    I : Integer;
begin
    for I := 0 to Pending.Count - 1 do begin
        ImageRec(Pending.Items[I]).Free;
    end;
    Pending.Clear;
    HttpStopSessions;
end;

procedure THTTPForm.CacheImagesClick(Sender : TObject);
begin
    CacheImages.Checked := not CacheImages.Checked;
end;

procedure THTTPForm.CachePagesClick(Sender : TObject);
begin
    CachePages.Checked := not CachePages.Checked;
end;

{ ----------------THTTPForm.CancelButtonClick }
procedure THTTPForm.CancelButtonClick(Sender : TObject);
begin
    AnAbort := True;
    if Assigned(Connection) then
        Connection.Abort;
    ClearProcessing;
    CheckEnableControls;
end;

{ ----------------THTTPForm.HistoryChange }
procedure THTTPForm.HistoryChange(Sender : TObject);
{ OnHistoryChange handler -- history list has changed }
var
    I   : Integer;
    Cap : String;
begin
    with Sender as TFrameBrowser do begin
        if History.Count > 0 then
            UrlComboBox.Text := History[0]; // ANGUS
        { check to see which buttons are to be enabled }
        FwdButton.Enabled  := FwdButtonEnabled;
        BackButton.Enabled := BackButtonEnabled;

        { Enable and caption the appropriate history menuitems }
        HistoryMenuItem.Visible := History.Count > 0;
        for I                   := 0 to MaxHistories - 1 do
            with Histories[I] do
                if I < History.Count then begin
                    Cap := History.Strings[I]; { keep local file name }
                    if TitleHistory[I] <> '' then
                        Cap := Cap + '--' + TitleHistory[I];
                    Caption := Copy(Cap, 1, 80);
                    Visible := True;
                    Checked := I = HistoryIndex;
                end
                else
                    Histories[I].Visible := False;
        Caption := DocumentTitle; { keep the Form caption updated }
        FrameBrowser.SetFocus;
    end;
end;

{ ----------------THTTPForm.HistoryClick }
procedure THTTPForm.HistoryClick(Sender : TObject);
{ A history list menuitem got clicked on }
var
    I : Integer;
begin
    { Changing the HistoryIndex loads and positions the appropriate document }
    I       := (Sender as TMenuItem).Tag;
    URLBase := GetUrlBase(FrameBrowser.History.Strings[I]);
    { update URLBase for new document }
    FrameBrowser.HistoryIndex := I;
    UrlComboBox.Text          := FrameBrowser.History[I]; // ANGUS
end;

{ ----------------THTTPForm.WMLoadURL }
procedure THTTPForm.WMLoadURL(var Message : TMessage);
begin
    GetButtonClick(Self);
end;

{ ----------------THTTPForm.Openfile1Click }
procedure THTTPForm.Openfile1Click(Sender : TObject);
{ Open a local disk file }
begin
    if CurrentLocalFile <> '' then
        OpenDialog.InitialDir := ExtractFilePath(CurrentLocalFile)
    else
        OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
    OpenDialog.FilterIndex    := 1;
    OpenDialog.Filename       := '';
    if OpenDialog.Execute then begin
        UrlComboBox.Text := 'file:///' + DosToHTMLNoSharp(OpenDialog.Filename);
        GetButtonClick(nil);
        Caption          := FrameBrowser.DocumentTitle;
        CurrentLocalFile := FrameBrowser.CurrentFile;
    end;
end;

{ ----------------THTTPForm.HotSpotTargetCovered }
procedure THTTPForm.HotSpotTargetCovered(Sender : TObject;
    const Target, URL : ThtString);
{ mouse moved over or away from a hot spot.  Change the status line }
begin
    if URL = '' then
        Status3.Caption := ''
    else if Target <> '' then
        Status3.Caption := 'Target: ' + Target + '  URL: ' + URL
    else
        Status3.Caption := 'URL: ' + URL
end;

{ ----------------THTTPForm.HotSpotTargetClick }
procedure THTTPForm.HotSpotTargetClick(Sender : TObject;
    const Target, URL : ThtString; var Handled : Boolean);
{ a link was clicked.  URL is a full url here have protocol and path added.  If
  you need the actual link String, it's available in the TFrameBrowser URL property }
const
    snd_Async = $0001; { play asynchronously }
var
    Protocol, Ext, FullUrl : String;
    PC                     : array [0 .. 255] of Char;
    S, Params              : String;
    K                      : Integer;
    Tmp                    : String;
begin
    LogLine('HotSpotTargetClick: ' + URL);
    if IsFullUrl(URL) then // ANGUS not always a full URL...
        FullUrl := URL
    else
        FullUrl := CombineURL(URLBase, URL);
    Protocol    := GetProtocol(FullUrl);
    if Protocol = 'mailto' then begin
        Tmp := FullUrl + #0; { for Delphi 1 }
        { call mail program }
        { Note: ShellExecute causes problems when run from Delphi 4 IDE }
        ShellExecute(Handle, nil, @Tmp[1], nil, nil, SW_SHOWNORMAL);
        Handled := True;
        exit;
    end;
    { Note: it would be nice to handle ftp protocol here also as some downloads use
      this protocol }

    Ext := LowerCase(GetURLExtension(FullUrl));
    if Pos('http', Protocol) > 0 then begin
        if (CompareText(Ext, 'zip') = 0) or (CompareText(Ext, 'exe') = 0) or
            (CompareText(Ext, 'pdf') = 0) or (CompareText(Ext, 'iso') = 0) or
            (CompareText(Ext, 'doc') = 0) or (CompareText(Ext, 'xls') = 0) then
        begin
            { download can't be done here.  Post a message to do it later at WMDownload }
            DownLoadUrl := FullUrl;
            PostMessage(Handle, wm_DownLoad, 0, 0);
            Handled := True;
            exit;
        end;
    end;
    if (Protocol = 'file') then begin
        S := FullUrl;
        K := Pos(' ', S); { look for parameters }
        if K = 0 then
            K := Pos('?', S); { could be '?x,y' , etc }
        if K > 0 then begin
            Params := Copy(S, K + 1, 255); { save any parameters }
            SetLength(S, K - 1); { truncate S }
        end
        else
            Params := '';
        S := HTMLToDos(S);
        if Ext = 'wav' then begin
            Handled := True;
            sndPlaySound(StrPCopy(PC, S), snd_Async);
        end
        else if Ext = 'exe' then begin
            Handled := True;
            StartProcess(StrPCopy(PC, S + ' ' + Params), sw_Show);
        end
        else if (Ext = 'mid') or (Ext = 'avi') then begin
            Handled := True;
            StartProcess(StrPCopy(PC, 'MPlayer.exe /play /close ' + S), sw_Show);
        end;
        { else ignore other extensions }
    end;
end;

procedure THTTPForm.ViewerClear(Sender : TObject);
{ A ThtmlViewer is about to be cleared. Cancel any image processing destined for it }
var
    I  : Integer;
    Vw : ThtmlViewer;
begin
    Vw    := Sender as ThtmlViewer;
    for I := Pending.Count - 1 downto 0 do
        with ImageRec(Pending.Items[I]) do
            if Viewer = Vw then begin
                Free;
                Pending.Delete(I);
            end;
end;

procedure THTTPForm.DeleteCacheClick(Sender : TObject);
begin
    DiskCache.EraseCache;
end;

procedure THTTPForm.wmDropFiles(var Message : TMessage);
{ handles dragging of file into browser window }
var
    S     : String;
    Count : Integer;
begin
    Count := DragQueryFile(message.WPARAM, 0, @S[1], 200);
    SetLength(S, Count);
    DragFinish(message.WPARAM);
    if Count > 0 then begin
        UrlComboBox.Text := 'file:///' + DosToHTMLNoSharp(S);
        GetButtonClick(nil);
        CurrentLocalFile := FrameBrowser.CurrentFile;
    end;
    message.Result := 0;
end;

procedure THTTPForm.Exit1Click(Sender : TObject);
begin
    Close;
end;

procedure THTTPForm.ShowDiagWindowClick(Sender : TObject);
begin
    ShowDiagWindow.Checked := not ShowDiagWindow.Checked;
    LogForm.Visible        := ShowDiagWindow.Checked;
end;

procedure THTTPForm.ShowImagesClick(Sender : TObject);
begin
    FrameBrowser.ViewImages := not FrameBrowser.ViewImages;
    ShowImages.Checked      := FrameBrowser.ViewImages;
end;

procedure THTTPForm.ShowLogHTMLClick(Sender : TObject);
begin
    ShowLogHTML.Checked := not ShowLogHTML.Checked;
end;

procedure THTTPForm.ShowLogHTTPClick(Sender: TObject);
begin
    ShowLogHTTP.Checked := not ShowLogHTTP.Checked;
end;

procedure THTTPForm.ReloadClick(Sender : TObject);
{ the Reload button was clicked }
begin
    ReloadButton.Enabled := False;
    Reloading            := True;
    FrameBrowser.Reload;
    ReloadButton.Enabled := True;
    FrameBrowser.SetFocus;
end;

procedure THTTPForm.WMDownLoad(var Message : TMessage);
{ Handle download of file }
var
    DownLoadForm : TDownLoadForm;
begin
    SaveDialog.Filename   := GetURLFilenameAndExt(DownLoadUrl);
    SaveDialog.InitialDir := Cache;
    if SaveDialog.Execute then begin
        DownLoadForm := TDownLoadForm.Create(Self);
        try
            with DownLoadForm do begin
                Filename    := SaveDialog.Filename;
                DownLoadUrl := Self.DownLoadUrl;
                Proxy       := Self.Proxy;
                ProxyPort   := Self.ProxyPort;
                UserAgent   := UserAgent;
                ShowModal;
            end;
        finally
            DownLoadForm.Free;
        end;
    end;
end;

procedure THTTPForm.FormShow(Sender : TObject);
{ OnShow handler.  Handles loading when a new instance is initiated by WinExec }
var
    S : String;
    I : Integer;
begin
    if (ParamCount >= 1) then begin { Parameter is file to load }
        S := CmdLine;
        I := Pos('" ', S);
        if I > 0 then
            Delete(S, 1, I + 1) { delete EXE name in quotes }
        else
            Delete(S, 1, Length(ParamStr(0))); { in case no quote marks }
        I := Pos('"', S);
        while I > 0 do { remove any quotes from parameter }
        begin
            Delete(S, I, 1);
            I := Pos('"', S);
        end;
        S := Trim(S);
        if not IsFullUrl(S) then
            S            := 'file:///' + DosToHtml(S);
        UrlComboBox.Text := Trim(S); { Parameter is URL to load }
        PostMessage(Handle, wm_LoadURL, 0, 0);
    end
{$IFDEF M4Viewer}
    else begin
        SelectStartingMode;
        if M4ConfigDeleteCache then
            DiskCache.EraseCache;
        UrlComboBox.Text := startfile;
        SendMessage(Handle, wm_LoadURL, 0, 0);
    end;
{$ELSE}
    else begin
        UrlComboBox.Text := 'res:///page0.htm';
        SendMessage(Handle, wm_LoadURL, 0, 0);
    end;
{$ENDIF}
end;

{ ----------------THTTPForm.BlankWindowRequest }
procedure THTTPForm.BlankWindowRequest(Sender : TObject;
    const Target, URL : ThtString);
{ OnBlankWindowRequest handler.  Either a Target of _blank or an unknown target
  was called for.  Load a new instance }
var
    S : String;
begin
    S := URL;
    if not IsFullUrl(S) then
        S := CombineURL(URLBase, S);

    S := ParamStr(0) + ' "' + S + '"';
    StartProcess(PChar(S), sw_Show);
end;

procedure THTTPForm.Find1Click(Sender : TObject);
begin
    FindDialog.Execute;
end;

procedure THTTPForm.FindDialogFind(Sender : TObject);
begin
    with FindDialog do begin
        if not FrameBrowser.FindEx(FindText, frMatchCase in Options,
            not(frDown in Options)) then
            MessageDlg('No further occurances of "' + FindText + '"',
                mtInformation, [mbOK], 0);
    end;
end;

procedure THTTPForm.Edit1Click(Sender : TObject);
begin
    with FrameBrowser do begin
        Copy1.Enabled      := SelLength <> 0;
        SelectAll1.Enabled := (ActiveViewer <> nil) and
            (ActiveViewer.CurrentFile <> '');
        Find1.Enabled := SelectAll1.Enabled;
    end;
end;

procedure THTTPForm.SelectAll1Click(Sender : TObject);
begin
    FrameBrowser.SelectAll;
end;

procedure THTTPForm.Copy1Click(Sender : TObject);
begin
    FrameBrowser.CopyToClipboard;
end;

procedure THTTPForm.URLComboBoxKeyPress(Sender : TObject; var Key : Char);
{ trap CR in combobox }
begin
    if (Key = #13) and (UrlComboBox.Text <> '') then begin
        Key := #0;
        GetButtonClick(Self);
    end;
end;

procedure THTTPForm.URLComboBoxClick(Sender : TObject);
begin
    if UrlComboBox.Text <> '' then
        GetButtonClick(Self);
end;

{ ----------------THTTPForm.RightClick }
procedure THTTPForm.RightClick(Sender : TObject;
    Parameters : TRightClickParameters);
{ OnRightClick handler.  Bring up popup menu allowing saving of image or opening
  a link in another window }
var
    Pt         : TPoint;
    S, Dest    : String;
    I          : Integer;
    Viewer     : ThtmlViewer;
    HintWindow : ThtHintWindow;
    ARect      : TRect;
begin
    Viewer := Sender as ThtmlViewer;
    with Parameters do begin
        FoundObject := Image;
        if (FoundObject <> nil) and (FoundObject.Graphic <> nil) then begin     // 11.6 was Bitmap
            if not IsFullUrl(FoundObject.Source) then
                FoundObjectName :=
                    CombineURL(FrameBrowser.GetViewerUrlBase(Viewer),
                    FoundObject.Source)
            else
                FoundObjectName := FoundObject.Source;
            SaveImageAs.Enabled := True;
        end
        else
            SaveImageAs.Enabled := False;

        if URL <> '' then begin
            S := URL;
            I := Pos('#', S);
            if I >= 1 then begin
                Dest := System.Copy(S, I, 255); { local destination }
                S    := System.Copy(S, 1, I - 1); { the file name }
            end
            else
                Dest := ''; { no local destination }
            if S = '' then
                S := Viewer.CurrentFile;
            if IsFullUrl(S) then
                NewWindowFile := S + Dest
            else
                NewWindowFile :=
                    CombineURL(FrameBrowser.GetViewerUrlBase(Viewer), S) + Dest;
            OpenInNewWindow.Enabled := True;
        end
        else
            OpenInNewWindow.Enabled := False;

        GetCursorPos(Pt);
        if Length(CLickWord) > 0 then begin
            HintWindow := ThtHintWindow.Create(Self);
            try
                ARect := Rect(0, 0, 0, 0);
                DrawText(HintWindow.Canvas.Handle, @CLickWord[1],
                    Length(CLickWord), ARect, DT_CALCRECT);
                with ARect do
                    HintWindow.ActivateHint
                        (Rect(Pt.X + 20, Pt.Y - (Bottom - Top) - 15,
                        Pt.X + 30 + Right, Pt.Y - 15), CLickWord);
                PopupMenu.Popup(Pt.X, Pt.Y);
            finally
                HintWindow.Free;
            end;
        end
        else
            PopupMenu.Popup(Pt.X, Pt.Y);
    end;
end;

procedure THTTPForm.SaveImageAsClick(Sender : TObject);
{ response to popup menu selection to save image }
var
    Stream      : TMemorystream;
    S           : String;
    DocType     : ThtmlFileType;
    AConnection : TURLConnection;
    Dummy       : String;
begin
    SaveDialog.InitialDir := Cache;
    SaveDialog.Filename   := GetURLFilenameAndExt(FoundObjectName);
    if SaveDialog.Execute then begin
        Stream := TMemorystream.Create;
        try
            if DiskCache.GetCacheFilename(FoundObjectName, S, DocType, Dummy)
            then begin
                Stream.LoadFromFile(S);
                Stream.SaveToFile(SaveDialog.Filename);
            end
            else begin
                AConnection := TURLConnection.GetConnection(FoundObjectName);
                if AConnection <> nil then
                    try
                        AConnection.InputStream := Stream;
                        AConnection.Get(FoundObjectName);
                        Stream.SaveToFile(SaveDialog.Filename);
                    finally
                        AConnection.Free;
                    end;
            end;
        finally
            Stream.Free;
        end;
    end;
end;

procedure THTTPForm.OpenInNewWindowClick(Sender : TObject);
{ response to popup menu selection to open link }
var
    PC : array [0 .. 255] of Char;
begin
    StartProcess(StrPCopy(PC, ParamStr(0) + ' "' + NewWindowFile + '"'), sw_Show);
end;

procedure THTTPForm.SaveURLClick(Sender : TObject);
{ put the entry in the combobox in the list.  It will be saved on exit }
begin
    with UrlComboBox do begin
        if Items.IndexOf(Text) < 0 then
            Items.Add(Text);
    end;
end;

procedure THTTPForm.HTTPDocData1(Sender : TObject; Buffer : Pointer;
    Len : Integer);
begin
    Status1.Caption := 'Text: ' + IntToStr(Connection.RcvdCount) + ' bytes';
    Status1.Update;
    Progress(Connection.RcvdCount, Connection.ContentLength);
end;

procedure THTTPForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
    if Assigned(Connection) then
        Connection.Abort;
    ClearProcessing;
end;

procedure THTTPForm.Progress(Num, Den : Integer);
var
    Percent : Integer;
begin
    if Den = 0 then
        Percent := 0
    else
        Percent    := (100 * Num) div Den;
    Gauge.Position := Percent;
    Gauge.Update;
end;

procedure THTTPForm.Status2Resize(Sender : TObject);
begin
    Gauge.SetBounds(5, 7, Status2.ClientWidth - 10, Status2.ClientHeight - 14);
end;

procedure THTTPForm.CheckException(Sender : TObject; E : Exception);
begin
    if E is ESpecialException then begin
        ShowMessage(E.Message);
        AnAbort := False;
    end
    else
        Application.ShowException(E);
end;


procedure THTTPForm.DisableControls;
begin
    UrlComboBox.Enabled  := False;
    CancelButton.Enabled := True;
    ReloadButton.Enabled := False;
    Animate1.Visible     := True;
    Animate1.Play(1, Animate1.FrameCount, 0);
    Gauge.Visible := True;
end;

procedure THTTPForm.EnableControls;
begin
    UrlComboBox.Enabled  := True;
    CancelButton.Enabled := False;
    ReloadButton.Enabled := FrameBrowser.CurrentFile <> '';
    Reloading            := False;
    Animate1.Active      := False;
    Animate1.Visible     := False;
    Gauge.Visible        := False;
end;

procedure THTTPForm.BackButtonClick(Sender : TObject);
begin
    FrameBrowser.GoBack;
    if FrameBrowser.HistoryIndex > FrameBrowser.History.Count then
        UrlComboBox.Text := FrameBrowser.History[FrameBrowser.HistoryIndex];
    // ANGUS
    CheckEnableControls;
end;

procedure THTTPForm.FwdButtonClick(Sender : TObject);
begin
    FrameBrowser.GoFwd;
    if FrameBrowser.HistoryIndex > FrameBrowser.History.Count then
        UrlComboBox.Text := FrameBrowser.History[FrameBrowser.HistoryIndex];
    // ANGUS
    CheckEnableControls;
end;

{ ----------------TImageHTTP.CreateIt }
constructor TImageHTTP.CreateIt(AOwner : TComponent; IRec : TObject);
begin
    inherited Create(AOwner);
    ImRec                  := IRec as ImageRec;
    Connection             := THTTPConnection.Create;
    Connection.CheckInputStream;   // creates stream
end;

{ ----------------TImageHTTP.GetAsync }
procedure TImageHTTP.GetAsync;
begin
    Connection.GetAsync(URL);
end;

{ ----------------TImageHTTP.Destroy }
destructor TImageHTTP.Destroy;
begin
    if Assigned(ImRec) then begin
        ImRec.Free;
    end;
    Connection.Free;
    inherited Destroy;
end;

{ ----------------THTTPForm.Processing }
procedure THTTPForm.Processing(Sender : TObject; ProcessingOn : Boolean);
begin
    if ProcessingOn then
    begin { disable various buttons and menuitems during processing }
        FwdButton.Enabled    := False;
        BackButton.Enabled   := False;
        ReloadButton.Enabled := False;
    end
    else begin
        FwdButton.Enabled    := FrameBrowser.FwdButtonEnabled;
        BackButton.Enabled   := FrameBrowser.BackButtonEnabled;
        ReloadButton.Enabled := FrameBrowser.CurrentFile <> '';
        CheckEnableControls;
    end;
end;

procedure THTTPForm.PrintPreviewClick(Sender : TObject);
var
{$IFDEF UseOldPreviewForm}
  pf: TPreviewForm;
{$ELSE UseOldPreviewForm}
  pf: TBegaHtmlPrintPreviewForm;
{$ENDIF UseOldPreviewForm}
  Viewer: ThtmlViewer;
  Abort: Boolean;
begin
  Viewer := FrameBrowser.ActiveViewer;
  if Assigned(Viewer) then
  begin
{$IFDEF UseOldPreviewForm}
    pf := TPreviewForm.CreateIt(Self, Viewer, Abort);
{$ELSE UseOldPreviewForm}
    pf := TBegaHtmlPrintPreviewForm.Create(Self);
    pf.FrameViewer := FrameBrowser;
    Abort := False;
{$ENDIF UseOldPreviewForm}
    try
      if not Abort then
         pf.ShowModal;
    finally
         pf.Free;
    end;
  end;
end;

procedure THTTPForm.File1Click(Sender : TObject);
begin
    Print1.Enabled := FrameBrowser.ActiveViewer <> nil;
    PrintPreview.Enabled := Print1.Enabled;
end;

procedure THTTPForm.Print1Click(Sender : TObject);
begin
    with PrintDialog do
        if Execute then
            if PrintRange = prAllPages then
                FrameBrowser.Print(1, 9999)
            else
                FrameBrowser.Print(FromPage, ToPage);
end;

procedure THTTPForm.PrintHeader(Sender : TObject; Canvas : TCanvas;
    NumPage, W, H : Integer; var StopPrinting : Boolean);
var
    AFont : TFont;
begin
    AFont      := TFont.Create;
    AFont.Name := FontSans;
    AFont.Size := 8;
    with Canvas do begin
        Font.Assign(AFont);
        SetBkMode(Handle, Transparent);
        SetTextAlign(Handle, TA_Bottom or TA_Left);
        if FrameBrowser.ActiveViewer <> nil then begin
            TextOut(50, H - 5, FrameBrowser.ActiveViewer.DocumentTitle);
            SetTextAlign(Handle, TA_Bottom or TA_Right);
            TextOut(W - 50, H - 5, FrameBrowser.ActiveViewer.CurrentFile);
        end;
    end;
    AFont.Free;
end;

procedure THTTPForm.PrintFooter(Sender : TObject; Canvas : TCanvas;
    NumPage, W, H : Integer; var StopPrinting : Boolean);
var
    AFont : TFont;
begin
    AFont      := TFont.Create;
    AFont.Name := FontSans;
    AFont.Size := 8;
    with Canvas do begin
        Font.Assign(AFont);
        SetTextAlign(Handle, TA_Bottom or TA_Left);
        TextOut(50, 20, DateToStr(Date));
        SetTextAlign(Handle, TA_Bottom or TA_Right);
        TextOut(W - 50, 20, 'Page ' + IntToStr(NumPage));
    end;
    AFont.Free;
end;

procedure THTTPForm.FrameBrowserMeta(Sender : TObject;
    const HttpEq, Name, Content : ThtString);
begin
    LogLine('FrameBrowserMeta, HttpEq=' + HttpEq + ', MetaName=' + name +
        ', MetaContent=' + Content);
end;

procedure THTTPForm.FrameBrowserScript(Sender : TObject;
    const Name, ContentType, Src, Script : ThtString);
begin
    LogLine('FrameBrowserScript, Name=' + name + ', Src=' + Src + ', Script='
        + Script);
end;

procedure THTTPForm.About1Click(Sender : TObject);
var
    AboutBox: TAboutBox;
begin
    AboutBox := TAboutBox.CreateIt(Self, 'FrameBrowser ICSv7/v8 Demo',
        'TFrameBrowser');
    try
        AboutBox.ShowModal;
    finally
        AboutBox.Free;
    end;
end;

procedure THTTPForm.Proxy1Click(Sender : TObject);
begin
    ProxyForm                    := TProxyForm.Create(Self);
    ProxyForm.ProxyEdit.Text     := Proxy;
    ProxyForm.PortEdit.Text      := ProxyPort;
    ProxyForm.ProxyUsername.Text := ProxyUser;
    ProxyForm.ProxyPassword.Text := ProxyPassword;
    ProxyForm.UserAgent.Text     := UserAgent;
    try
        if ProxyForm.ShowModal = mrOK then begin
            Proxy         := ProxyForm.ProxyEdit.Text;
            ProxyPort     := ProxyForm.PortEdit.Text;
            ProxyUser     := ProxyForm.ProxyUsername.Text;
            ProxyPassword := ProxyForm.ProxyPassword.Text;
            UserAgent     := ProxyForm.UserAgent.Text;
        end;
    finally
        ProxyForm.Free;
    end;
end;

procedure THTTPForm.DemoInformation1Click(Sender : TObject);
begin
    UrlComboBox.Text := 'res:///page0.htm';
    GetButtonClick(Self);
end;

procedure THTTPForm.FrameBrowserMouseMove(Sender : TObject; Shift : TShiftState;
    X, Y : Integer);
var
    TitleStr : String;
begin
    if not Timer1.Enabled and (Sender is ThtmlViewer) and
        Assigned(ActiveControl) and ActiveControl.Focused then begin
        TitleViewer := ThtmlViewer(Sender);
        TitleStr    := TitleViewer.TitleAttr;
        if TitleStr = '' then
            OldTitle := ''
        else if TitleStr <> OldTitle then begin
            TimerCount     := 0;
            Timer1.Enabled := True;
            OldTitle       := TitleStr;
        end;
    end;
end;

procedure THTTPForm.CloseHints;
begin
    Timer1.Enabled := False;
    HintWindow.ReleaseHandle;
    HintVisible := False;
    TitleViewer := nil;
end;

procedure THTTPForm.Timer1Timer(Sender : TObject);
const
    StartCount = 2; { timer counts before hint window opens }
    EndCount   = 20; { after this many timer counts, hint window closes }
var
    Pt, Pt1  : TPoint;
    ARect    : TRect;
    TitleStr : ThtString;

begin
    if not Assigned(TitleViewer) then begin
        CloseHints;
        exit;
    end;
    Inc(TimerCount);
    GetCursorPos(Pt);
    try { in case TitleViewer becomes corrupted }
        Pt1      := TitleViewer.ScreenToClient(Pt);
        TitleStr := TitleViewer.TitleAttr;
        if (TitleStr = '') or not PtInRect(TitleViewer.ClientRect, Pt1) then
        begin
            OldTitle := '';
            CloseHints;
            exit;
        end;
        if TitleStr <> OldTitle then begin
            TimerCount := 0;
            OldTitle   := TitleStr;
            HintWindow.ReleaseHandle;
            HintVisible := False;
            exit;
        end;

        if TimerCount > EndCount then
            CloseHints
        else if (TimerCount >= StartCount) and not HintVisible then begin
            ARect := HintWindow.CalcHintRect(300, TitleStr, nil);
            with ARect do
                HintWindow.ActivateHint(Rect(Pt.X, Pt.Y + 18, Pt.X + Right,
                    Pt.Y + 18 + Bottom), TitleStr);
            HintVisible := True;
        end;
        { note: this exception can occur when switching frames while TitleViewer is active.  It is
          adequately handled here and only appears in the IDE when "Stop on Delphi Exceptions" is
          turned on }
    except
        CloseHints;
    end;
end;

//-- BG ---------------------------------------------------------- 16.08.2015 --
procedure THTTPForm.AppMessage(var Msg: TMsg; var Handled: Boolean);
var
  WinCtrl: TWinControl;
begin
  if Msg.message = WM_MOUSEWHEEL then
  begin
    WinCtrl := FindVCLWindow(Point(Word(Msg.lParam), HiWord(Msg.lParam)));
    if (WinCtrl is TPaintPanel) {$ifndef UseOldPreviewForm} or (WinCtrl is TBegaZoomBox) {$endif UseOldPreviewForm} then
    begin
      // perform mouse wheel scrolling for the control under the mouse:
      WinCtrl.Perform(CM_MOUSEWHEEL, Msg.WParam, Msg.LParam);
      Handled := True;
    end;
  end;
end;

end.
