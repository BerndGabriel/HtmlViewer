{
Version   11.7
Copyright (c) 1995-2008 by L. David Baldwin,
Copyright (c) 2008-2016 by HtmlViewer Team

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
}
unit URLSubs;

{.$I htmlcons.inc}

interface

uses
{$ifdef MSWindows}
  Windows,
{$else MSWindows}
  Types,
{$endif MSWindows}
  HtmlGlobals;

type
  ThtDocType = (HTMLType, TextType, ImgType, XHtmlType, OtherType);

{***************************************************************************************************
 * URL processing methods
 **************************************************************************************************}

procedure ParseURL(const Url: ThtString; out Proto, User, Pass, Host, Port, Path: ThtString);
{François PIETTE's URL parsing procedure}

procedure SplitString(var Str: ThtString; Sep: ThtChar; out Spall: ThtString); overload;
{$ifndef UNICODE}
procedure SplitString(var Str: String; Sep: Char; out Spall: String); overload;
{$endif}
{Split Str at first appearance of Sep into Str and Spall. Spall starts with Sep.}

function IsValidSchemeChar(const Ch: ThtChar): Boolean;
{ Returns true, if Ch is a valid scheme char (a..z, A..Z, 0..9, +, -, .)}
function FindSchemeSep(const Src: ThtString; var Index: Integer): Boolean;
{Find the ':' after a series of valid scheme chars starting with Src[Index].
 Returns true, if there is a ':' only preceeded by valid scheme chars.
 If returns true Index is advanced to the position of ':'}

procedure SplitScheme(const Src: ThtString; out Name, SchemeSpecific: ThtString); overload;
procedure SplitScheme(var Src: ThtString; out SchemeSpecific: ThtString); overload;
function  SplitScheme(var Src: ThtString): ThtString; overload;
{Split an URL into scheme and scheme specific part at first appearance of ':'}

procedure SplitDest(const Src: ThtString; out Name, Dest: ThtString); overload;
procedure SplitDest(var Src: ThtString; out Dest: ThtString); overload;
function  SplitDest(var Src: ThtString): ThtString; overload;
{Split local destination from an URL at first appearance of '#', returns Dest with separating '#'}

procedure SplitQuery(const Src: ThtString; out Name, Query: ThtString); overload;
procedure SplitQuery(var Src: ThtString; out Query: ThtString); overload;
function  SplitQuery(var Src: ThtString): ThtString; overload;
{$ifndef UNICODE}
procedure SplitQuery(var Src: String; out Query: String); overload;
function  SplitQuery(var Src: String): String; overload;
{$endif}
{Split query from an URL at first appearance of '?', returns Query with separating '?'}

function GetBase(const URL: ThtString): ThtString; deprecated;
function GetURLBase(const URL: ThtString): ThtString;
{Given an URL, get the base directory}

function Combine(Base, APath: ThtString): ThtString; deprecated;
function CombineURL(Base, APath: ThtString): ThtString;
{combine a base and a path taking into account that overlap might exist}
{needs work for cases where directories might overlap}

function IsFullURL(const URL: ThtString): boolean;
{set if contains a protocol}

function GetProtocol(const URL: ThtString): ThtString;
{return the http, mailto, etc in lower case}

function GetURLExtension(const URL: ThtString): ThtString;
{returns extension without the '.', mixed case}

function GetURLFilenameAndExt(const URL: ThtString): ThtString;
{returns mixed case after last /}

function DecodeURL(const URL: ThtString): ThtString;
{return % encoded URL converted to UnicodeString}

function EncodeUrl(const Url: ThtString): ThtString;


{***************************************************************************************************
 * both URL and DOS processing methods
 **************************************************************************************************}

function Normalize(const URL: ThtString): ThtString;
{lowercase, trim, and make sure a '/' terminates a hostname, adds http://}

function DosToHTML(const FName: ThtString): ThtString;
{convert an Dos style filename to one for HTML.  Does not add the file:///}

function DosToHtmlNoSharp(FName: ThtString): ThtString;
{convert a Dos style filename to one for HTML.  Does not add the file:///.
 use where "#" is not being used as a local position but rather is part of filename.}

function HTMLToDos(const FName: ThtString): ThtString;
{convert an HTML style filename to one for Dos}

function HTMLServerToDos(const FName, Root: ThtString): ThtString;
{convert an HTML style filename to one for Dos. Prefix with Root, if name starts with a single backslash}

{***************************************************************************************************
 * DOS processing methods
 **************************************************************************************************}

function IsAbsolutePath(const Filename: ThtString): Boolean;
{set if contains a drive or server}

function CombineDos(const Base, Path: ThtString): ThtString;
{combine a base and a path}

{***************************************************************************************************
 * both URL and Resource processing methods
 **************************************************************************************************}

function HTMLToRes(const URL: ThtString; out AType: ThtString): ThtString;
{convert an (% encoded) URL like res:[/]*<name>.<type> to resource name and type}

{**************************************************************************************************}
implementation

uses
  SysUtils, Math;

{----------------GetURLBase}

function GetURLBase(const URL: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
{Given an URL, get the base directory}
var
  I, J, Q: integer;
begin
  Result := Trim(URL);
  Q := Pos('?', Result);
  if Q = 0 then
    Q := Length(Result);
  J := Pos('//', Result);
  if (J = 0) or (J > Q) then
    J := 1;
  for I := Q downto J do
    if Result[I] = '/' then
    begin
      SetLength(Result, I);
      Exit;
    end;
  Result := Result + '/';
end;

function GetBase(const URL: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
begin
  Result := GetURLBase(URL);
end;

{----------------Combine}
function CombineDos(const Base, Path: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
const
  DirSep =
{$ifdef MSWindows}
    '\';
{$endif MSWindows}
{$ifdef Unix}
    '/';
{$endif Unix}

var
  L: Integer;
begin
  L := Length(Base);
  if (L > 0) and (Base[L] = DirSep) then
    if (Length(Path) > 0) and (Path[1] = DirSep) then
      Result := Copy(Base, 1, L - 1) + Path
    else
      Result := Base + Path
  else
    if (Length(Path) > 0) and (Path[1] = DirSep) then
      Result := Base + Path
    else
      Result := Base + DirSep + Path;
end;

function Combine(Base, APath: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
begin
  Result := CombineURL(Base, APath);
end;

function CombineURL(Base, APath: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
{combine a base and a path taking into account that overlap might exist}
{needs work for cases where directories might overlap}
var
  I, J, K: integer;
  Proto: ThtString;
begin
  Base := Trim(Base);
  Proto := GetProtocol(Base);
  J := Pos('file:///', Base);
  if J = 1 then
    J := Pos('/', Copy(Base, 9, Length(Base) - 8)) + 8 {fourth slash for file:///c|/ }
  else
  begin
    J := Pos('://', Base);
    if J > 0 then
      J := Pos('/', Copy(Base, J + 3, Length(Base) - (J + 2))) + J + 2 {third slash for http://www.xxx.com/ }
    else
      J := Pos('/', Base);
  end;
{insure there is a '/' on the end}
  if J = 0 then
  begin
    Base := Base + '/'; {needs a slash}
    J := Length(Base);
  end
  else if Base[Length(Base)] <> '/' then
    Base := Base + '/';

  APath := Trim(APath);

  if (Length(APath) >= 1) and (APath[1] = '/') then
  begin {remove path from base and use host only}
    if (Length(APath) >= 2) and (APath[2] = '/') then
      // '//...' is an absolue path, but not an UNC path, because DosToHtml()
      // had detected it as an absolute path and thus had added a scheme/protocol
      // and thus CombineURL had not been called.
      if Proto = 'file' then
        Result := 'http:' + APath
      else
        Result := Proto + ':' + APath
    else
      Result := Copy(Base, 1, J) + Copy(APath, 2, Length(APath) - 1);
  end
  else
    Result := Base + APath;

{remove any '..\'s to simply and standardize for cacheing}
  I := Pos('/../', Result);
  while I > 0 do
  begin
    if I > J then
    begin
      K := I;
      while (I > 1) and (Result[I - 1] <> '/') do
        Dec(I);
      if I <= 1 then
        Break;
      Delete(Result, I, K - I + 4); {remove canceled directory and '/../'}
    end
    else
      Delete(Result, I + 1, 3); {remove '../' after host name}
    I := Pos('/../', Result);
  end;
{remove any './'s}
  I := Pos('/./', Result);
  while I > 0 do
  begin
    Delete(Result, I + 1, 2);
    I := Pos('/./', Result);
  end;
end;

function Normalize(const URL: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
{trim, and make sure a '/' terminates a hostname and http:// is present.
 In other words, if there is only 2 /'s, put one on the end}
var
  I, J, LastSlash: integer;
  Query: ThtString;
begin
  Result := Trim(URL);
  if Pos('mailto:', Result) = 1 then
    Exit;
  J := Pos('?', Result);
  if J > 0 then
  begin
    Query := Copy(Result, J, Length(Result) - J + 1);
    Delete(Result, J, Length(Result) - J + 1);
  end
  else
    Query := '';
  if Pos('file://', Result) = 1 then
    Result := DosToHTML(Result);
  if GetProtocol(Result) = '' then
    Result := 'http://' + Result; {add http protocol as a default}
  J := Pos('/./', Result);
  while J > 0 do
  begin
    Delete(Result, J + 1, 2); {remove './'s}
    J := Pos('/./', Result);
  end;
  J := Pos('//', Result);
  LastSlash := 0;
  for I := J + 2 to Length(Result) do
    if Result[I] = '/' then
      LastSlash := I;
  if LastSlash = 0 then
    Result := Result + '/';
  Result := Result + Query;
end;

function IsFullURL(const URL: ThtString): Boolean;
 {$ifdef UseInline} inline; {$endif}
var
  I: integer;
begin
  I := 1;
  Result := FindSchemeSep(URL, I);
end;

//-- BG ---------------------------------------------------------- 28.06.2015 --
function IsAbsolutePath(const Filename: ThtString): Boolean;
begin
{$ifdef MSWindows}
  Result := (Length(Filename) >= 2) and ((Filename[2] = ':') or ((Filename[1] = '\') and (Filename[2] = '\')));
{$endif}
{$ifdef Unix}
  Result := (Length(Filename) > 0) and (Filename[1] = '/');
{$endif}
end;

function GetProtocol(const URL: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
var
  User, Pass, Port, Host, Path: ThtString;
  S: ThtString;
  I: integer;
begin
  I := Pos('?', URL);
  if I > 0 then
    S := Copy(URL, 1, I - 1)
  else
    S := URL;
  ParseURL(S, Result, user, pass, Host, port, Path);
  Result := LowerCase(Result);
end;

function GetURLExtension(const URL: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
var
  I, N: integer;
begin
  Result := '';
  I := Pos('?', URL);
  if I > 0 then
    N := I - 1
  else
    N := Length(URL);
  I := Pos('#', URL);
  if (I > 0) and (I < N) then
    N := I - 1;
  for I := N downto Max(1, N - 5) do
    if URL[I] = '.' then
    begin
      Result := Copy(URL, I + 1, N - I);
      Break;
    end;
end;

function GetURLFilenameAndExt(const URL: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
var
  I: integer;
begin
  Result := URL;
  for I := Length(URL) downto 1 do
    if URL[I] = '/' then
    begin
      Result := Copy(URL, I + 1, 255);
      Break;
    end;
end;

// Syntax of an URL: protocol://[user[:password]@]server[:port]/path
// Thanx to François PIETTE

procedure ParseURL(
  const Url: ThtString;
  out Proto, User, Pass, Host, Port, Path: ThtString);

  // Find the count'th occurence of string s in string t.
  // If count < 0 then look from the back
  // Thanx to François PIETTE

  function Posn(const s, t: ThtString; Count: Integer): Integer;
  var
    i, h: Integer;
    u: ThtString;
  begin
    if Count > 0 then
    begin
      u := t;
      Result := Length(u);
      for i := 1 to Count do
      begin
        h := Pos(s, u);
        if h > 0 then
          u := Copy(u, h + 1, Length(u))
        else
        begin
          u := '';
          Inc(Result);
        end;
      end;
      Result := Result - Length(u);
    end
    else if Count < 0 then
    begin
      Result := 0;
      // BG, 21.08.2011: just a little optimizing:
      // - cannot match, if copy of t is shorter than s: start with i at Length(t) - Length(s) + 1.
      // - cannot match, if 1st char already does not match: skip the string copy and pos.
      for i := Length(t) - Length(s) + 1 downto 1 do
        if t[i] = s[1] then
        begin
          u := Copy(t, i, Length(t));
          h := Pos(s, u);
          if (h <> 0) and ((h + i) <> Result) then
          begin
            Result := h + i - 1;
            Inc(Count);
            if Count = 0 then
              break;
          end;
        end;
      if Count <> 0 then
        Result := 0;
    end
    else
      Result := 0;
  end;

var
  n, p, q: Integer;
  s: ThtString;
begin
  SetLength(Proto, 0);
  SetLength(User , 0);
  SetLength(Pass , 0);
  SetLength(Host , 0);
  SetLength(Port , 0);
  SetLength(Path , 0);

  n := Length(Url);
  if n < 1 then
    Exit;

  p := Pos('://', Url);
  if p = 0 then
  begin
    if (url[1] = '/') then
    begin
      { Absolute path without protocol specified }
      proto := 'http';
      p := 1;
      if (Length(url) > 1) and (url[2] <> '/') then
      begin
                { Relative path }
        Path := Copy(url, 1, Length(url));
        Exit;
      end;
    end
    else if lowercase(Copy(url, 1, 5)) = 'http:' then
    begin
      proto := 'http';
      p := 6;
      if (Length(url) > 6) and (url[7] <> '/') then
      begin
        { Relative path }
        Path := Copy(url, 6, Length(url));
        Exit;
      end;
    end
    else if lowercase(Copy(url, 1, 7)) = 'mailto:' then
    begin
      proto := 'mailto';
      p := pos(':', url);
    end
    else if lowercase(Copy(url, 1, 5)) = 'data:' then
    begin
      proto := 'data';
      p := pos(':', url);
    end
    else
    begin
      p := 1;
      if FindSchemeSep(Url, p) then
        Proto := Copy(Url, 1, p - 1);
    end;
  end
  else
  begin
    proto := Copy(url, 1, p - 1);
    inc(p, 2);
  end;
  s := Copy(url, p + 1, Length(url));

  p := pos('/', s);
  if p = 0 then
    p := Length(s) + 1;
  Path := Copy(s, p, Length(s));
  s := Copy(s, 1, p - 1);

  p := Posn(':', s, -1);
  if p > Length(s) then
    p := 0;
  q := Posn('@', s, -1);
  if q > Length(s) then
    q := 0;
  if (p = 0) and (q = 0) then
  begin { no user, password or port }
    Host := s;
    Exit;
  end
  else if q < p then
  begin { a port given }
    Port := Copy(s, p + 1, Length(s));
    Host := Copy(s, q + 1, p - q - 1);
    if q = 0 then
      Exit; { no user, password }
    s := Copy(s, 1, q - 1);
  end
  else
  begin
    Host := Copy(s, q + 1, Length(s));
    s := Copy(s, 1, q - 1);
  end;
  p := pos(':', s);
  if p = 0 then
    User := s
  else
  begin
    User := Copy(s, 1, p - 1);
    Pass := Copy(s, p + 1, Length(s));
  end;
end;

//-- BG ---------------------------------------------------------- 28.11.2010 --
procedure SplitString(var Str: ThtString; Sep: ThtChar; out Spall: ThtString);
 {$ifdef UseInline} inline; {$endif}
// Extracted from several locations spread all over the code.
// Splits Str at position of Sep into Str and Spall. Spall starts with Sep.
// If Sep is not in Str, on return Str is unchanged and Spall is empty.
var
  I: Integer;
begin
  I := Pos(Sep, Str) - 1; // '-1' will be needed in both copy()s and comparing with 0 is faster.
  if I >= 0 then
  begin
    Spall := System.Copy(Str, I + 1, Length(Str) - I);
    SetLength(Str, I);
  end
  else
    Spall := '';
end;

{$ifndef UNICODE}
procedure SplitString(var Str: String; Sep: Char; out Spall: String);
 {$ifdef UseInline} inline; {$endif}
// Extracted from several locations spread all over the code.
// Splits Str at position of Sep into Str and Spall. Spall starts with Sep.
// If Sep is not in Str, on return Str is unchanged and Spall is empty.
var
  I: Integer;
begin
  I := Pos(Sep, Str) - 1; // '-1' will be needed in both copy()s and comparing with 0 is faster.
  if I >= 0 then
  begin
    Spall := System.Copy(Str, I + 1, Length(Str) - I);
    SetLength(Str, I);
  end
  else
    Spall := '';
end;
{$endif}

//-- BG ---------------------------------------------------------- 28.06.2015 --
function IsValidSchemeChar(const Ch: ThtChar): Boolean;
 {$ifdef UseInline} inline; {$endif}
begin
  case Ch of
    '0'..'9',
    'a'..'z',
    'A'..'Z',
    '+', '-', '.':
      Result := True;
  else
    Result := False;
  end;
end;

//-- BG ---------------------------------------------------------- 28.06.2015 --
function FindSchemeSep(const Src: ThtString; var Index: Integer): Boolean;
var
  I, N: Integer;
begin
  I := Index;
  N := Length(Src);
  while (I <= N) and IsValidSchemeChar(Src[I]) do
    Inc(I);

  // Assuming that all scheme names are longer than 1 char (as 1 char might indicate a DOS drive name):
  Result := (I > 2) and (I <= N) and (Src[I] = ':');
  if Result then
    // Yes there is a scheme:
    Index := I;
end;


//-- BG ---------------------------------------------------------- 18.05.2016 --
function SplitScheme(var Src: ThtString): ThtString; overload;
begin
  SplitScheme(Src, Result);
end;

//-- BG ---------------------------------------------------------- 28.06.2015 --
procedure SplitScheme(var Src: ThtString; out SchemeSpecific: ThtString); overload;
var
  I: Integer;
begin
  I := 1;
  if FindSchemeSep(Src, I) then
  begin
    SchemeSpecific := Copy(Src, I + 1, MaxInt);
    SetLength(Src, I - 1);
  end
  else
  begin
    SchemeSpecific := Src;
    SetLength(Src, 0);
  end;
end;

//-- BG ---------------------------------------------------------- 28.06.2015 --
procedure SplitScheme(const Src: ThtString; out Name, SchemeSpecific: ThtString); overload;
 {$ifdef UseInline} inline; {$endif}
{Split an URL into scheme and scheme specific part}
var
  I: Integer;
begin
  I := 1;
  if FindSchemeSep(Src, I) then
  begin
    Name := Copy(Src, 1, I - 1);
    SchemeSpecific := Copy(Src, I + 1, MaxInt);
  end
  else
  begin
    SchemeSpecific := Src;
    SetLength(Name, 0);
  end;
end;


//-- BG ---------------------------------------------------------- 18.05.2016 --
procedure SplitDest(var Src: ThtString; out Dest: ThtString); overload; {$ifdef UseInline} inline; {$endif}
begin
  SplitString(Src, ThtChar('#'), Dest);
end;

function SplitDest(var Src: ThtString): ThtString; overload; {$ifdef UseInline} inline; {$endif}
begin
  SplitDest(Src, Result);
end;

procedure SplitDest(const Src: ThtString; out Name, Dest: ThtString); overload; {$ifdef UseInline} inline; {$endif}
{Split an URL into filename and Destination}
begin
  Name := Src;
  SplitDest(Name, Dest);
end;


//-- BG ---------------------------------------------------------- 18.05.2016 --
procedure SplitQuery(var Src: ThtString; out Query: ThtString); overload; {$ifdef UseInline} inline; {$endif}
begin
  SplitString(Src, ThtChar('?'), Query);
end;

function SplitQuery(var Src: ThtString): ThtString; overload; {$ifdef UseInline} inline; {$endif}
begin
  SplitQuery(Src, Result);
end;

procedure SplitQuery(const Src: ThtString; out Name, Query: ThtString); overload; {$ifdef UseInline} inline; {$endif}
begin
  Name := Src;
  SplitQuery(Name, Query);
end;

{$ifndef UNICODE}
procedure SplitQuery(var Src: String; out Query: String); overload;
 {$ifdef UseInline} inline; {$endif}
begin
  SplitString(Src, '?', Query);
end;

function SplitQuery(var Src: String): String; overload;
 {$ifdef UseInline} inline; {$endif}
begin
  SplitString(Src, '?', Result);
end;
{$endif}


function DosToHTML(const FName: ThtString): ThtString;
{convert a Dos style filename to one for HTML.  Does not add the file:///}
var
  Colon: integer;

  procedure Replace(Old, New: ThtChar);
  var
    I: integer;
  begin
    I := Pos(Old, Result);
    while I > 0 do
    begin
      Result[I] := New;
      I := Pos(Old, Result);
    end;
  end;

begin
  Result := FName;
  Colon := Pos('://', Result);
  Replace(':', '|');
  Replace('\', '/');
  if Colon > 0 then
    Result[Colon] := ':'; {return it to a colon}
end;

function DosToHtmlNoSharp(FName: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
{convert a Dos style filename to one for HTML.  Does not add the file:///.
 use where "#" is not being used as a local position but rather is part of filename.}
var
  I: integer;
begin
  I := Pos('#', FName);
  while I > 0 do
  begin
    Delete(FName, I, 1);
    Insert('%23', FName, I);
    I := Pos('#', FName);
  end;
  Result := FName;
end;

//-- BG ---------------------------------------------------------- 04.04.2012 --
function DecodeURL(const URL: ThtString): ThtString;
{return % encoded URL converted to UnicodeString.
 According to http://tools.ietf.org/html/rfc3986 percent encoded data is in UTF-8}
var
  J: Integer;
  FName: ThtString;

  function GetNextChar: Cardinal;

    function GetNext: Cardinal;

      function CharToHexInt(Chr: ThtChar): Cardinal;
      begin
        case Chr of
          '0'..'9': Result := Ord(Chr) - Ord('0');
          'A'..'F': Result := Ord(Chr) - Ord('A') + 10;
          'a'..'f': Result := Ord(Chr) - Ord('a') + 10;
        else
          raise EConvertError.Create('Invalid escape character: ''' + Char(Chr) + '''');
        end;
      end;

    begin
      if Length(FName) - J < 2 then
        raise EConvertError.CreateFmt('Invalid escape sequence: 3 digits expected, but %d found', [Length(FName) - J + 1]);
      if FName[J] <> '%' then
        raise EConvertError.CreateFmt('Invalid escape sequence: "%%" expected but "%s" (#%d) found', [FName[J], Ord(FName[J])]);
      Inc(J);

      Result := CharToHexInt(FName[J]) * 16;
      Inc(J);

      Inc(Result, CharToHexInt(FName[J]));
      Inc(J);
    end;

  var
    Chr: Cardinal;
  begin
    // convert UTF-8 to Unicode:
    Result := GetNext;
    if (Result and $80) <> 0 then
    begin
      Chr := Result and $3F;
      if (Result and $20) <> 0 then
      begin
        Result := GetNext;
        if (Result and $C0) <> $80 then
          raise EConvertError.Create('Invalid 3 octet UTF sequence.');
        Chr := (Chr shl 6) or (Result and $3F);
      end;
      Result := GetNext;
      if (Result and $C0) <> $80 then
        raise EConvertError.Create('Invalid UTF sequence.');
      Result := (Chr shl 6) or (Result and $3F);
    end;
  end;

var
  I: integer;
begin
  FName := URL;
  I := Pos('%', FName);
  if I > 0 then
    while I <= Length(FName) - 2 do
    begin
      if FName[I] = '%' then
      begin
        try
          J := I;
          FName[I] := ThtChar(GetNextChar);
          Inc(I);
          Delete(FName, I, J - I);
        except {ignore exception}
          I := J;
        end;
      end
      else
        Inc(I);
    end;
  Result := FName;
end;

//- BG ----------------------------------------------------------- 01.11.2006 --
function EncodeUrl(const Url: ThtString): ThtString;

  function CharToHex(Ch: ThtChar): ThtString;
  begin
    Result := '%' + IntToHex(Ord(Ch), 2);
  end;

var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 1 to Length(Url) do
    if Pos(Url[I], ' *<>#%"{}|\^[]`+') > 0 then
      htAppendStr(Result, CharToHex(Url[I]))
    else
      htAppendChr(Result, Url[I]);
end;

function HTMLToDos(const FName: ThtString): ThtString;
{convert an HTML style filename to one for Dos}
var
  I, N: integer;

  procedure Replace(Old, New: ThtChar);
  var
    I: integer;
  begin
    I := Pos(Old, Result);
    while I > 0 do
    begin
      Result[I] := New;
      I := Pos(Old, Result);
    end;
  end;

begin
  Result := DecodeURL(FName);
  I := Pos('/', Result);
  if I <> 0 then
  begin
    I := Pos('file:/', Lowercase(Result));
    if I > 0 then
    begin
      N := Length(Result);
      if (N < 7) or (Result[7] <> '/') then
        N := 6
      else if (N < 8) or (Result[8] <> '/') then
        N := 7
      else
        N := 8;
      System.Delete(Result, I, N);
    end;
    Replace('|', ':');
{$ifdef MSWindows}
    Replace('/', '\');
{$endif}
  end;
end;

function HTMLServerToDos(const FName, Root: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
{Add Prefix Root only if first character is '\' but not '\\'}
begin
{$ifdef MSWindows}
  Result := Trim(HTMLToDos(FName));
  if (Result <> '') and (Root <> '') then
  begin
    if Pos('\\', Result) = 1 then
      Exit;
    if Pos(':', Result) = 2 then
      Exit;
    if Result[1] = '\' then
      Result := Root + Result;
  end;
{$endif}
{$ifdef Unix}
  Result := Trim(FName);
  if not IsAbsolutePath(Result) and (Length(Root) > 0) then
    Result := Root + Result;
{$endif}
end;

//-- BG ---------------------------------------------------------- 02.04.2014 --
function HTMLToRes(const URL: ThtString; out AType: ThtString): ThtString;
{convert an URL like res:[/]*<name>.<type> to resource name and type
  Any number of '/' between 'res:' and 'name.ext' are allowed and ignored.
  On return Result is <name> and AType is <type>.
}
var
  I: integer;

begin
  Result := DecodeURL(URL);

  I := 1;
  if htLowerCase(Copy(Result, 1, 4)) = 'res:' then
    I := 5;

  while (I <= Length(Result)) and (Result[I] = '/') do
    Inc(I);

  if I > 1 then
    System.Delete(Result, 1, I - 1);

  SetLength(AType, 0);
  for I := Length(Result) downto 1 do
    if Result[I] = '.' then
    begin
      AType := Copy(Result, I + 1, MaxInt);
      SetLength(Result, I - 1);
      break;
    end;

end;

end.

