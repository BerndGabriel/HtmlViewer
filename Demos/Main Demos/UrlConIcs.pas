{
Version   11.7
Copyright (c) 2016 by HtmlViewer Team

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

Note that the source modules HTMLGIF1.PAS, DITHERUNIT.PAS
are covered by separate copyright notices located in those modules.
}
unit UrlConIcs;

interface
{
Inspired by the former UrlConIcs.PAS:

Copyright (c) 2012 by Angus Robertson delphi@magsys.co.uk

UrlConIcs.PAS evolved from UrlConId.PAS written by Yves Urbain.
}
// This is the original copyright notice of UrlConId.PAS:
{ ********************************************************* }
{ *                     UrlConId.PAS                      * }
{ *                Copyright (c) 1999 by                  * }
{ *                   Metaphor SPRL                       * }
{ *                 All rights reserved.                  * }
{ *                Written by Yves Urbain                 * }
{ ********************************************************* }
{ *                                                       * }
{ * March 2012 - Angus Robertson replaced Indy with ICSv7 * }
{ *      Suspect this program originally used ICS since   * }
{ *      many types, vars and events have ICS names       * }
{ *                                                       * }
{ * April 2012 - Angus simplified State                   * }
{ *                                                       * }
{ * May 2016 - Bernd introduced ThtConnector and          * }
{ *      ThtConnection                                    * }
{ *                                                       * }
{ ********************************************************* }

uses
{$ifdef LCL}
  LCLIntf, LCLType, LMessages,
{$else}
  ShellAPI, WinTypes, WinProcs,
{$endif}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, TypInfo,
  HtmlGlobals, URLSubs, UrlConn,
  OverbyteIcsWndControl, OverbyteIcsWSocket, OverbyteIcsWinsock,
  OverbyteIcsHttpProt, OverbyteIcsHttpCCodzlib, OverbyteIcsCookies,
  OverbyteIcsUtils, OverbyteIcsStreams, OverbyteIcsMimeUtils;

type

  ThtIcsTranslateStringEvent = function(const AURL: string): string of object;

  THTTPConnection = class(ThtConnection)
  private
    FHttp: TSslHttpCli;

    FUrlBase: string;
    FCurRawSubmitValues: ThtStringList;
    FCurSubmitCodepage: LongWord;
    FAllow: string;
    FNewLocation: string;
    FResponseText: string;
    FResponseCode: Integer;

    FOnGetCookie: ThtIcsTranslateStringEvent;
    FOnGetMimeTypeForFile: ThtIcsTranslateStringEvent;

    procedure HttpRedirect(Sender : TObject);
    procedure HttpDocBegin(Sender: TObject);
    procedure HttpDocData(Sender : TObject; Buffer : Pointer; Len : Integer);
    procedure HttpDocEnd(Sender: TObject);
  protected
    procedure IcsHttpStateChange(Sender : TObject);
    procedure IcsHttpHeaderData(Sender : TObject);
    procedure IcsHttpCommand(Sender : TObject; var S : String);
    procedure IcsHttpSessionConnected(Sender : TObject);
    procedure IcsHttpSessionClosed(Sender : TObject);

    property OnGetCookie: ThtIcsTranslateStringEvent read FOnGetCookie write FOnGetCookie;
    property OnGetMimeTypeForFile: ThtIcsTranslateStringEvent read FOnGetMimeTypeForFile write FOnGetMimeTypeForFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Get(Doc: ThtUrlDoc); override;
    procedure Abort; override;
//    procedure Close; override;  // ANGUS
    property StatusCode        : Integer                read FResponseCode;
  end;

  ThtIcsHttpConnector = class(ThtProxyConnector)
  private
    FSslContext: TSslContext;
    FIcsCookies: TIcsCookies;
    FMimeTypes: TMimeTypesList;

    FUserAgent: string;
    FCookieFile: String;
    function StoreUserAgent: Boolean;
    procedure HTTPSetCookie(Sender: TObject; const Data: String; var Accept: Boolean);
    procedure IcsCookiesNewCookie(Sender: TObject; ACookie: TCookie; var Save: Boolean);
  protected
    class function GetDefaultProtocols: string; override;
    class function GetVersion: string; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure LoadCookies;
    procedure SaveCookies;

    function CreateConnection(const Protocol: String): ThtConnection; override;
  published
    property OnGetAuthorization;
    property UserAgent: string read FUserAgent write FUserAgent stored StoreUserAgent;
    property CookieFile: String read FCookieFile write FCookieFile;
    property CookieManager: TIcsCookies read FIcsCookies write FIcsCookies;
    property SslContext: TSslContext read FSslContext write FSslContext;
    property MimeTypes: TMimeTypesList read FMimeTypes write FMimeTypes;
  end;

implementation

uses
  LogFormUnit,
  Registry;

const
  CUserAgent = 'Mozilla/4.0 (compatible; MSIE 5.0; Windows 98)';

var
  GLmCompatLevel : LongWord; { AG }

function GetLMCompatLevel : integer; { AG }
const
    LSA_KEY    = 'System\CurrentControlSet\Control\LSA';
    VALUE_NAME = 'LMCompatibilityLevel';
begin
    Result := 0;
    try
        with TRegistry.Create(KEY_READ) do
        try
            RootKey := HKEY_LOCAL_MACHINE;
            if KeyExists(LSA_KEY) then begin
                if OpenKey(LSA_KEY, False) and ValueExists(VALUE_NAME) then
                    Result := LongWord(ReadInteger(VALUE_NAME));
            end;
        finally
            Free;
        end;
    except
    end;
end;

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

{ ThtIcsHttpConnector }

//-- BG ---------------------------------------------------------- 29.05.2016 --
constructor ThtIcsHttpConnector.Create(AOwner: TComponent);
begin
  inherited;
  FSslContext := TSslContext.Create(Self);
  FMimeTypes  := TMimeTypesList.Create(Self);
  FUserAgent  := CUserAgent;
end;

//-- BG ---------------------------------------------------------- 29.05.2016 --
function ThtIcsHttpConnector.CreateConnection(const Protocol: String): ThtConnection;
var
  Connection: THTTPConnection absolute Result;
begin
  if FIcsCookies = nil then
    LoadCookies;

  Result := THTTPConnection.Create;
  Connection.FHttp.SslContext    := FSslContext;
  Connection.FHttp.OnCookie      := HttpSetCookie;
  Connection.FHttp.Agent         := UserAgent;
  Connection.FHttp.Proxy         := ProxyServer;
  Connection.FHttp.ProxyPort     := ProxyPort;
  Connection.FHttp.ProxyUsername := ProxyUsername;
  Connection.FHttp.ProxyPassword := ProxyPassword;
  if (ProxyUsername <> '') and (ProxyPassword <> '') then
    Connection.FHttp.ProxyAuth   := httpAuthBasic;

  Connection.OnGetCookie          := FIcsCookies.GetCookies;
  Connection.OnGetMimeTypeForFile := FMimeTypes.TypeFromFile;
end;

//-- BG ---------------------------------------------------------- 29.05.2016 --
procedure ThtIcsHttpConnector.HTTPSetCookie(Sender : TObject; const Data : String; var Accept : Boolean);
begin
  FIcsCookies.SetCookie(Data, (Sender as TSslHttpCli).URL);
end;

//-- BG ---------------------------------------------------------- 29.05.2016 --
procedure ThtIcsHttpConnector.IcsCookiesNewCookie(Sender : TObject; ACookie : TCookie; var Save : Boolean);
var
  S : String;
begin
  if LogForm.LogActive[laDiag] then
  begin
    S := 'NewCookie: ' + ACookie.CName + '=' + ACookie.CValue + ', Domain=' + ACookie.CDomain + ', Path=' + ACookie.CPath;
    if ACookie.CPersist then
      S := S + ', Expires=' + DateTimeToStr(ACookie.CExpireDT)
    else
      S := S + ', Not Persisent';
    if ACookie.CSecureOnly then
      S := S + ', SecureOnly';
    if ACookie.CHttpOnly then
      S := S + ', HttpOnly';
    LogForm.LogSynced(S);
  end;
end;

//-- BG ---------------------------------------------------------- 29.05.2016 --
class function ThtIcsHttpConnector.GetDefaultProtocols: string;
begin
  Result := 'http,https';
end;

//-- BG ---------------------------------------------------------- 29.05.2016 --
class function ThtIcsHttpConnector.GetVersion: string;
begin
  Result := Format('ICS %d.%.02d', [TIcsWndControlVersion div 100, TIcsWndControlVersion mod 100]);
end;

//-- BG ---------------------------------------------------------- 29.05.2016 --
procedure ThtIcsHttpConnector.LoadCookies;
begin
  if FIcsCookies = nil then
  begin
    FIcsCookies := TIcsCookies.Create(Self);
    FIcsCookies.OnNewCookie := IcsCookiesNewCookie;
  end;
  FIcsCookies.LoadFromFile(FCookieFile);
  FIcsCookies.AutoSave := True;
end;

//-- BG ---------------------------------------------------------- 29.05.2016 --
procedure ThtIcsHttpConnector.SaveCookies;
begin
  if FIcsCookies <> nil then
    if Length(FCookieFile) > 0 then
      if not FIcsCookies.AutoSave then
        FIcsCookies.SaveToFile(FCookieFile);
end;

//-- BG ---------------------------------------------------------- 29.05.2016 --
function ThtIcsHttpConnector.StoreUserAgent: Boolean;
begin
  Result := FUserAgent <> CUserAgent;
end;

{ ----------------THTTPConnection.Create }
constructor THTTPConnection.Create;
begin
  inherited Create;
  WSocketForceLoadWinsock;

  FResponseCode := 404;

  FHttp := TSslHttpCli.Create(nil);
  FHttp.FollowRelocation   := True;
  FHttp.RequestVer         := '1.1';
  FHttp.Connection         := 'Keep-Alive';
  FHttp.LmCompatLevel      := GLmCompatLevel; { AG }
  FHttp.OnLocationChange   := HttpRedirect;
  FHttp.OnDocBegin         := HttpDocBegin;
  FHttp.OnDocData          := HttpDocData;
  FHttp.OnDocEnd           := HttpDocEnd;
  FHttp.OnHeaderData       := IcsHttpHeaderData;
  FHttp.OnCommand          := IcsHttpCommand;
  FHttp.OnSessionConnected := IcsHttpSessionConnected;
  FHttp.OnSessionClosed    := IcsHttpSessionClosed;
  FHttp.OnStateChange      := IcsHttpStateChange;
end;

{ ----------------THTTPConnection.Destroy }
destructor THTTPConnection.Destroy;
begin
  FHttp.Free;
  inherited Destroy;
end;

{ ----------------THTTPConnection.Get }
procedure THTTPConnection.Get(Doc: ThtUrlDoc);
const
  MaxRedirect = 15;
var
  PostIt1, TryAgain, TryRealm: Boolean;
  SendStream: TStream;
  RedirectCount: Integer;
  Url1, Query1, S, Filename, RandBoundary, InputValue, InputName, Header, Footer: String;
{$ifdef UNICODE}
  AnsiQuery: AnsiString;
{$endif}
  I, SLen : Integer;
begin
  FHttp.Reference := Doc.Referer;
  Url1 := Doc.Url;
  Query1 := Doc.Query;
  PostIt1 := Doc.PostIt;
  FUrlBase := GetUrlBase(URL1);
  RedirectCount := -1;
  repeat
    TryRealm := True;
    TryAgain := False;
    Inc(RedirectCount);
    Doc.Clear;
    try
      Aborted := False;
      //FHttp.ServerAuth := BasicAuthentication;
      FHttp.Username := Username;
      FHttp.Password := Password;
      try
        if PostIt1 then
        begin { Post }
          if Doc.QueryEncType = 'multipart/form-data' then { AG }
          begin
            Filename     := '';
            RandBoundary := IntToHex(Random(MaxInt), 8);
            FHttp.ContentTypePost := Doc.QueryEncType + '; boundary=---------------------------' + RandBoundary;
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
                  OnGetMimeTypeForFile(InputValue)
                  + CRLF + CRLF;
              end
              else
              begin
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

            if Length(Header) > 0 then
              Footer := CRLF + Footer;
            Footer := Footer + '-----------------------------' +
              RandBoundary + '--' + CRLF;

            if Length(Filename) > 0 then // if it's a file upload
            begin
              SendStream := TMultiPartFileReader.Create(Filename,
                               UnicodeToAnsi(Header, FCurSubmitCodepage, True),
                               UnicodeToAnsi(Footer, FCurSubmitCodepage, True));
            end
            else
              SendStream := TMemorystream.Create;

            try
                if LogForm.LogActive[laDiag] then
                  LogForm.LogSynced('FrameBrowser Post: ' + URL1 + ', EncType=' + Doc.QueryEncType);
                if Filename = '' then
                // No file upload, write to stream
                begin
  {$ifdef UNICODE}
                    AnsiQuery := UnicodeToAnsi(Footer, FCurSubmitCodepage);
                    SendStream.WriteBuffer (Pointer(AnsiQuery)^,
                                                            Length(AnsiQuery));
  {$else UNICODE}
  {$endif UNICODE}

                end;
                SendStream.Position := 0;
                FHttp.SendStream := SendStream;
                FHttp.URL        := URL1;
                FHttp.Post; // sync
            finally
                SendStream.Free;
            end;
          end // EncType = 'multipart/form-data'
          else
          begin
            SendStream := TMemorystream.Create;
            try
              if LogForm.LogActive[laDiag] then
                  LogForm.LogSynced('FrameBrowser Post: ' + URL1 + ', Data=' + Copy(Query1, 1, 1024) + ', EncType=' + Doc.QueryEncType);
              // not too much data

              AnsiQuery := AnsiString(Query1);
              if Length(Doc.QueryEncType) > 0 then
                FHttp.ContentTypePost := Doc.QueryEncType
              else
                FHttp.ContentTypePost := 'application/x-www-form-urlencoded';
              SendStream.WriteBuffer (AnsiQuery[1], Length(AnsiQuery));

              SendStream.Position := 0;
              FHttp.SendStream := SendStream;
              FHttp.URL        := URL1;
              FHttp.Post; // sync
            finally
              SendStream.Free;
            end;
          end;
        end
        else
        begin { Get }
          if Length(Query1) > 0 then
            Url1 := Url1 + '?' + Query1;
          if LogForm.LogActive[laDiag] then
            LogForm.LogSynced('THTTPConnection.Get Get: ' + Url1);
          FHttp.RcvdStream := Doc.Stream;
          FHttp.URL        := Doc.URL;
          FHttp.Get; // sync
        end
      finally
        Doc.DocType   := ContentType2DocType(FHttp.ContentType);
        ReceivedSize  := FHttp.ContentLength;
        FResponseText := FHttp.ReasonPhrase;
        FResponseCode := FHttp.StatusCode;
        if FHttp.AuthorizationRequest.Count > 0 then { AG }
        begin
          { the 'realm' here is just used for user/password lookups }
          S := FHttp.AuthorizationRequest[0];
          if StrIComp('NTLM', PChar(S)) = 0 then
            { there is no realm in NTLM }
            Realm := 'NTLM @ ' + FHttp.Hostname
          else if StrLIComp(PChar(S), 'BASIC REALM="', 13) = 0 then
          begin
            I    := 14;
            SLen := Length(S);
            while (I <= SLen) and (S[I] <> '"') do
                Inc(I);
            if S[I] = '"' then
            begin
              //FBasicAuth := True;
              Realm     := Copy(S, 1, I) + ' @ ' + FHttp.Hostname;
            end;
          end
          else
          begin
            if StrLIComp(PChar(S), 'DIGEST REALM="', 14) = 0 then begin
              I    := 15;
              SLen := Length(S);
              while (I <= SLen) and (S[I] <> '"') do
                Inc(I);
              if S[I] = '"' then
                Realm := Copy(S, 1, I) + ' @ ' + FHttp.Hostname;
            end;
          end;
        end;
      end;
      if FResponseCode = 401 then
      begin
        TryAgain := GetAuthorization(TryRealm);
        TryRealm := False;
      end;
    except
      case FResponseCode of
        401:
          begin
            TryAgain := GetAuthorization(TryRealm);
            TryRealm := False;
          end;

        405:
          if PostIt1 and (Pos('get', FAllow) > 0) then
          begin
            PostIt1  := False;
            TryAgain := True;
          end;

        301, 302:
          if FNewLocation <> '' then
          begin
            URL1     := FNewLocation;
            TryAgain := True;
          end;
      end;
      if not TryAgain or (RedirectCount >= MaxRedirect) then
        raise;
    end;
    if LogForm.LogActive[laDiag] then
      LogForm.LogSynced('FrameBrowserGetPostRequestEx Done: Status ' + IntToStr(FResponseCode));
  until not TryAgain;
end;

//-- BG ---------------------------------------------------------- 29.05.2016 --
procedure THTTPConnection.HttpDocBegin(Sender: TObject);
begin
  Doc.Status := ucsInProgress;
  ExpectedSize := FHttp.ContentLength;
  ReceivedSize := 0;

  if Assigned(OnDocBegin) then
    OnDocBegin(Self);
end;

//-- BG ---------------------------------------------------------- 29.05.2016 --
procedure THTTPConnection.HttpDocData(Sender : TObject; Buffer : Pointer; Len : Integer);
begin
  ReceivedSize := ReceivedSize + Len;
  ExpectedSize := FHttp.ContentLength;
  if Assigned(OnDocData) then
    OnDocData(Self);
end;

//-- BG ---------------------------------------------------------- 29.05.2016 --
procedure THTTPConnection.HttpDocEnd(Sender: TObject);
begin
  Doc.Status := ucsLoaded;
  if Assigned(OnDocEnd) then
    OnDocEnd(Self);
end;

//-- BG ---------------------------------------------------------- 29.05.2016 --
procedure THTTPConnection.HttpRedirect(Sender : TObject);
var
  Dest : String;
begin
  Dest := (Sender as TSslHttpCli).Location;
  if LogForm.LogActive[laDiag] then
    LogForm.LogSynced('Redirected to: ' + Dest);

  //Proto := GetProtocol(FUrlBase);
  if IsFullUrl(Dest) then { it's a full URL }
    FNewLocation := Normalize(Dest)
  else
    FNewLocation := CombineURL(FUrlBase, Dest);
  FUrlBase := GetUrlBase(FNewLocation);

  { cookies may have been sent during redirection, so update again now   }
  (Sender as TSslHttpCli).Cookie := OnGetCookie(FNewLocation);
end;

procedure THTTPConnection.IcsHttpHeaderData(Sender : TObject);
begin
  if LogForm.LogActive[laHttpHeader] then
    LogForm.LogSynced('[' + IntToStr(SessionId) + '] < ' + FHttp.LastResponse);
end;

procedure THTTPConnection.IcsHttpCommand(Sender : TObject; var S : String);
begin
  if LogForm.LogActive[laHttpHeader] then
    LogForm.LogSynced('[' + IntToStr(SessionId) + '] > ' + S);
end;

procedure THTTPConnection.IcsHttpSessionConnected(Sender : TObject);
begin
  if LogForm.LogActive[laHttpHeader] then
    LogForm.LogSynced('[' + IntToStr(SessionId) + '] Connected to ' + (Sender as TSslHttpCli).Hostname);
end;

procedure THTTPConnection.IcsHttpSessionClosed(Sender : TObject);
begin
  if LogForm.LogActive[laHttpHeader] then
    LogForm.LogSynced('[' + IntToStr(SessionId) + '] Disconnected');
end;

procedure THTTPConnection.IcsHttpStateChange(Sender : TObject);
var
  FState: THttpState;
begin
  FState := (Sender as TSslHttpCli).State;
  if LogForm.LogActive[laHttpHeader] then
    LogForm.LogSynced('[' + IntToStr(SessionId) + '] StateChange: ' + GetEnumName(Typeinfo(THttpState), Ord(FState)));
end;

{ ----------------THTTPConnection.Abort }
procedure THTTPConnection.Abort;
begin
  if Assigned(FHttp) then
    FHttp.Abort;
end;

//{ ----------------THTTPConnection.Close }
//procedure THTTPConnection.Close;
//begin
//  if Assigned(FHttp) then
//    if FState = httpReady then
//      FHttp.Close;
//end;

initialization

GLmCompatLevel := GetLMCompatLevel; { AG }

end.
