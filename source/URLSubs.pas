{
Version   11
Copyright (c) 1995-2008 by L. David Baldwin, 2008-2010 by HtmlViewer Team

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
  Windows,
  HtmlGlobals;

{***************************************************************************************************
 * URL processing methods
 **************************************************************************************************}

procedure ParseURL(const url: ThtString; out Proto, User, Pass, Host, Port, Path: ThtString);
{François PIETTE's URL parsing procedure}

procedure SplitString(var Str: ThtString; Sep: ThtChar; out Spall: ThtString);
{Split Str at first appearance of Sep into Str and Spall. Spall starts with Sep.}

procedure SplitDest(const Src: ThtString; out Name, Dest: ThtString); overload;
procedure SplitDest(var Src: ThtString; out Dest: ThtString); overload;
{Split local destination from an URL at first appearance of '#', returns Dest with separating '#'}

procedure SplitQuery(const Src: ThtString; out Name, Query: ThtString); overload;
procedure SplitQuery(var Src: ThtString; out Query: ThtString); overload;
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
{return % encoded URL convert to UnicodeString}

{***************************************************************************************************
 * both URL and DOS processing methods
 **************************************************************************************************}

function Normalize(const URL: ThtString): ThtString;
{lowercase, trim, and make sure a '/' terminates a hostname, adds http://}

function DosToHTML(FName: ThtString): ThtString;
{convert an Dos style filename to one for HTML.  Does not add the file:///}

function DosToHtmlNoSharp(FName: ThtString): ThtString;
{convert a Dos style filename to one for HTML.  Does not add the file:///.
 use where "#" is not being used as a local position but rather is part of filename.}

function HTMLToDos(FName: ThtString): ThtString;
{convert an HTML style filename to one for Dos}

function HTMLServerToDos(FName, Root: ThtString): ThtString;
{convert an HTML style filename to one for Dos. Prefix with Root, if name starts with a single backslash}

{***************************************************************************************************
 * DOS processing methods
 **************************************************************************************************}

function CombineDos(const Base, Path: ThtString): ThtString;
{combine a base and a path}


{**************************************************************************************************}
implementation

uses
  SysUtils, Math;

{----------------GetURLBase}

function GetURLBase(const URL: ThtString): ThtString;
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
begin
  Result := GetURLBase(URL);
end;

{----------------Combine}
function CombineDos(const Base, Path: ThtString): ThtString;
var
  L: Integer;
begin
  L := Length(Base);
  if (L > 0) and (Base[L] = '\') then
    if (Length(Path) > 0) and (Path[1] = '\') then
      Result := Copy(Base, 1, L - 1) + Path
    else
      Result := Base + Path
  else
    if (Length(Path) > 0) and (Path[1] = '\') then
      Result := Base + Path
    else
      Result := Base + '\' + Path;
end;

function Combine(Base, APath: ThtString): ThtString;
begin
  Result := CombineURL(Base, APath);
end;

function CombineURL(Base, APath: ThtString): ThtString;
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
    if (Length(APath) >= 2) and (APath[2] = '/') then {UNC filename}
      if Proto = 'file' then
        Result := 'file:///' + APath
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
{trim, and make sure a '/' terminates a hostname and http:// is present.
 In other words, if there is only 2 /'s, put one on the end}
var
  I, J, LastSlash: integer;
  Query: ThtString;
begin
  Result := Trim(URL);
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

function IsFullURL(const URL: ThtString): boolean;
var
  N: integer;
  S: ThtString;
begin
  N := Pos('?', Url);
  if N > 0 then
    S := Copy(Url, 1, N - 1)
  else
    S := Url;
  N := Pos('://', S);
  if (N > 0) and (N < Pos('/', S)) then
    Result := True
  else
  begin
    S := htLowerCase(S);
    Result := (Copy(S, 1, 7) = 'mailto:') or (Copy(S, 1, 5) = 'data:');
  end;
end;

function GetProtocol(const URL: ThtString): ThtString;
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
  Result := Lowercase(Result);
end;

function GetURLExtension(const URL: ThtString): ThtString;
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
  const url: ThtString;
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
  p, q: Integer;
  s: ThtString;
begin
  proto := '';
  User := '';
  Pass := '';
  Host := '';
  Port := '';
  Path := '';

  if Length(url) < 1 then
    Exit;

  p := pos('://', url);
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
// extracted from several locations spread all over the code.
var
  I: Integer;
begin
  I := Pos(Sep, Str) - 1; // '-1' will be needed in both copy()s and comparing with 0 is faster.
  if I >= 0 then
  begin
    Spall := System.Copy(Str, I + 1, Length(Str) - I);
    Str   := System.Copy(Str, 1, I);
  end
  else
    Spall := '';
end;

procedure SplitDest(var Src: ThtString; out Dest: ThtString); overload;
begin
  SplitString(Src, '#', Dest);
end;

procedure SplitDest(const Src: ThtString; out Name, Dest: ThtString); overload;
{Split an URL into filename and Destination}
begin
  Name := Src;
  SplitString(Name, '#', Dest);
end;

procedure SplitQuery(var Src: ThtString; out Query: ThtString); overload;
begin
  SplitString(Src, '?', Query);
end;

procedure SplitQuery(const Src: ThtString; out Name, Query: ThtString); overload;
begin
  Name := Src;
  SplitString(Name, '?', Query);
end;

function DosToHTML(FName: ThtString): ThtString;
{convert a Dos style filename to one for HTML.  Does not add the file:///}
var
  Colon: integer;

  procedure Replace(Old, New: ThtChar);
  var
    I: integer;
  begin
    I := Pos(Old, FName);
    while I > 0 do
    begin
      FName[I] := New;
      I := Pos(Old, FName);
    end;
  end;

begin
  Colon := Pos('://', FName);
  Replace(':', '|');
  Replace('\', '/');
  if Colon > 0 then
    FName[Colon] := ':'; {return it to a colon}
  Result := FName;
end;

function DosToHtmlNoSharp(FName: ThtString): ThtString;
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
{return % encoded URL convert to UnicodeString.
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
          raise EConvertError.Create('Invalid escape character: ''' + Chr + '''');
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

function HTMLToDos(FName: ThtString): ThtString;
{convert an HTML style filename to one for Dos}
var
  I, N: integer;

  procedure Replace(Old, New: ThtChar);
  var
    I: integer;
  begin
    I := Pos(Old, FName);
    while I > 0 do
    begin
      FName[I] := New;
      I := Pos(Old, FName);
    end;
  end;

begin
  FName := DecodeURL(FName);
  I := pos('/', FName);
  if I <> 0 then
  begin
    I := Pos('file:/', Lowercase(FName));
    if I > 0 then
    begin
      N := Length(FName);
      if (N < 7) or (FName[7] <> '/') then
        N := 6
      else if (N < 8) or (FName[8] <> '/') then
        N := 7
      else
        N := 8;
      System.Delete(FName, I, N);
    end;
    Replace('|', ':');
    Replace('/', '\');
  end;
  Result := FName;
end;

function HTMLServerToDos(FName, Root: ThtString): ThtString;
{Add Prefix Root only if first character is '\' but not '\\'}
begin
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
end;

end.
