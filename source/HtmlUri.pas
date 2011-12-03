{
Version   12
Copyright (c) 2011-2012 by Bernd Gabriel

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
unit HtmlUri;

{$I htmlcons.inc}

interface

uses
  HtmlGlobals;

{***************************************************************************************************
 * new URI processing methods. Incl. IPv6, UNC and DOS path recognition
 ***************************************************************************************************
 * about "Uniform Resource Identifier (URI): Generic Syntax" see http://tools.ietf.org/html/rfc3986
 **************************************************************************************************}

type
  // BG, 21.08.2011:
  TUri = record
    Scheme: ThtString;
    Username: ThtString;
    Password: ThtString;
    Host: ThtString;
    Port: ThtString;
    Path: ThtString;
    Query: ThtString;
    Fragment: ThtString;
  end;

//------------------------------------------------------------------------------
// elementary methods
//------------------------------------------------------------------------------

// Converts an URI string to a TUri record.
function StrToUri(Uri: ThtString): TUri; overload;

// Builds an absolute URI from RelativeUri by combining it with the given AbsoluteUri.
function CombineUri(RelativeUri, AbsoluteUri: TUri): TUri;

// Converts all parts of the Uri to their normalized forms.
// Converts to proper lettercase, Percent encodes reserved characters.
// Converts backslashs to slashs in file URIs.
procedure NormalizeUri(var Uri: TUri);

// Converts a record TUri to an URI string.
function UriToStr(Uri: TUri): ThtString;

// converts all percent encoded characters in the URI string back to the readable ones.
function DecodeUri(Uri: ThtString): ThtString;

//------------------------------------------------------------------------------
// convenience methods
//------------------------------------------------------------------------------

// Converts an URI string to a normalized TUri record.
function StrToUri(Str, DefaultScheme: ThtString): TUri; overload;

// Converts an URI string to a normalized URI string.
function StrToUriStr(Str, DefaultScheme: ThtString): ThtString; overload;


implementation

//-- BG ---------------------------------------------------------- 22.08.2011 --
function Next(const Uri: ThtString; var I: Integer): ThtChar; {$ifdef UseInline} inline; {$endif}
begin
  if I < Length(Uri) then
  begin
    Inc(I);
    Result := Uri[I];
    if Result = '\' then
      Result := '/';
  end
  else
    Result := #0;
end;

//-- BG ---------------------------------------------------------- 22.08.2011 --
function Chr2Hex(Ch: ThtChar): Integer;
begin
  case Ch of
    '0'..'9': Result := Ord(Ch) - Ord('0');
    'a'..'f': Result := Ord(Ch) - Ord('a') + 10;
    'A'..'F': Result := Ord(Ch) - Ord('A') + 10;
  else
    Result := 0;
  end;
end;

//-- BG ---------------------------------------------------------- 21.08.2011 --
function CombineUri(RelativeUri, AbsoluteUri: TUri): TUri;
begin

end;

//-- BG ---------------------------------------------------------- 22.08.2011 --
function DecodeUri(Uri: ThtString): ThtString;
// convert the percent encoded characters back to ansi/ascii chars.
var
  I, J: Integer;
  Ch: ThtChar;
begin
  SetLength(Result, Length(Uri));
  I := 0;
  J := 0;
  repeat
    Ch := Next(Uri, I);
    case Ch of
      '%':
      begin
        Ch := Next(Uri, I);
        Ch := ThtChar(Chr2Hex(Ch) shl 4 + Chr2Hex(Next(Uri, I)));
      end;

      #0:
        break;
    end;
    Inc(J);
    Result[J] := Ch;
  until False;
  SetLength(Result, J);
end;

//-- BG ---------------------------------------------------------- 21.08.2011 --
procedure NormalizeUri(var Uri: TUri);
// for better path normalization you should call NormalizeUri() after CombineUri().

  procedure percentNormalization(var Str: ThtString);
  // convert the unreserved percent encoded characters back to ansi/ascii chars.
  // convert remaining code chars to uppercase
  var
    I, J: Integer;
    Ch, Ch1, Ch2: ThtChar;
    Result: ThtString;
  begin
    SetLength(Result, Length(Str) * 3);
    I := 0;
    J := 0;
    repeat
      Ch := Next(Str, I);
      case Ch of
        '%':
        begin
          Ch1 := Next(Str, I);
          Ch2 := Next(Str, I);
          Ch := ThtChar(Chr2Hex(Ch1) shl 4 + Chr2Hex(Ch2));
          case Ch of
            // unreserved characters:
            'a'..'z', 'A'..'Z', '0'..'9',
            '-', '_', '.', '~': ;
          else
            Inc(J);
            Result[J] := '%';
            Inc(J);
            Result[J] := ThtChar(Ord(Ch1) - (Ord('a') - Ord('A')));
            Ch := ThtChar(Ord(Ch2) - (Ord('a') - Ord('A')))
          end;
        end;

        // unreserved characters:
        'a'..'z', 'A'..'Z', '0'..'9',
        '-', '_', '.', '~': ;

        #0:
          break;
      else
        // must be percent encoded:
        Inc(J);
        Result[J] := '%';
        Inc(J);
        Result[J] := ThtChar((Ord(Ch) and $F0) shr 4);
        Ch := ThtChar(Ord(Ch) and $F);
      end;
      Inc(J);
      Result[J] := Ch;
    until False;
    SetLength(Result, J);
    Str := Result;
  end;

  procedure pathNormalization(var Path: ThtString);
  var
    I, J, K, Len: Integer;
    Ch: ThtChar;
    Result: ThtString;
    IsSep: Boolean;
    SepPos: array of Integer;
  begin
    Len := Length(Path);
    if Len = 0 then
    begin
      Path := '/';
      Exit;
    end;

    Inc(Len);
    SetLength(Result, Len);
    SetLength(SepPos, Len);

    I := 0;
    J := 0;
    K := 0;
    SepPos[K] := 0;

    repeat
      Ch := Next(Path, I);
      case Ch of
        '/':
        begin
          IsSep := False;
          case Next(Path, I) of
            '.':
              case Next(Path, I) of
                '.':
                  case Next(Path, I) of
                    '/': // step back to
                    begin
                      Dec(I);
                      J := SepPos[K];
                      if K > 0 then
                        Dec(K);
                    end
                  else
                    Dec(I, 3);
                    IsSep := True;
                  end;

                '/': // skip '/.', continue with '/'
                begin
                  Dec(I);
                end;
              else
                Dec(I, 2);
                IsSep := True;
              end;

          else
            Dec(I);
            IsSep := True;
          end;

          if IsSep then
          begin
            if J > SepPos[K] then // preserve adjacent '//'
              Inc(K);
            SepPos[K] := J;
          end;
        end;

        #0:
          break;
      end;
      Inc(J);
      Result[J] := Ch;
    until False;
    SetLength(Result, J);
    Path := Result;
  end;

begin

  // 1. Case and Percent Normalization

  // - convert a..f in percent encoding to uppercase A..F
  // - do not %encode the unreserved chars ALPHA / DIGIT / "-" / "." / "_" / "~"
  percentNormalization(Uri.Username);
  percentNormalization(Uri.Password);
  percentNormalization(Uri.Path);
  percentNormalization(Uri.Query);
  percentNormalization(Uri.Fragment);

  // - convert scheme and host to lowercase
  Uri.Scheme := htLowerCase(Uri.Scheme);
  Uri.Host := htLowerCase(Uri.Host);

  // 2. Path Normalization

  // - eliminate /./ and /../
  // - replace empty path with /
  // - convert backslash in path to /
  pathNormalization(Uri.Path);

  // 3. Scheme Based Normalization

  if (Uri.Scheme = 'http') or (Uri.Scheme = 'https') then
  begin
    // 3.1 http

    // - remove default port 80
    if Uri.Port = '80' then
      Uri.Port := '';
  end;
end;

//-- BG ---------------------------------------------------------- 21.08.2011 --
function StrToUri(Uri: ThtString): TUri;

  function IsAlpha(Ch: ThtChar): Boolean; {$ifdef UseInline} inline; {$endif}
  begin
    case Ch of
      'a'..'z', 'A'..'Z':
        Result := True;
    else
      Result := False;
    end;
  end;

  function IsDigit(Ch: ThtChar): Boolean; {$ifdef UseInline} inline; {$endif}
  begin
    case Ch of
      '0'..'9':
        Result := True;
    else
      Result := False;
    end;
  end;

  function Copy(const Uri: ThtString; I, Len: Integer): ThtString;
  begin
    if Len > 0 then
      Result := System.Copy(Uri, I, Len)
    else
      Result := '';
  end;

var
  I, Beg: Integer;
  Ch: ThtChar;
  IsPort: Boolean;
begin
  I := 0;

  // parse scheme
  Result.Scheme := '';
  Ch := Next(Uri, I);
  Beg := I;
  if IsAlpha(Ch) then
  begin
    repeat
      case Ch of
        ':':
        begin
          // end of scheme or is it a DOS path?
          if I = 2 then
          begin
            // assume, it is a DOS path
            Result.Scheme := 'file';
            Uri := '///' + Uri;
            I := 1;
            Ch := Uri[I];
            break;
          end
          else
            Result.Scheme := Copy(Uri, Beg, I - Beg);
          Ch := Next(Uri, I);
          Beg := I;
          break;
        end;

        '/', '?', '#', #0:
          // scheme is omitted
          break;

      end;
      Ch := Next(Uri, I);
    until False;
  end;

  // parse hierarchy
  IsPort := False;
  Result.Username := '';
  Result.Password := '';
  Result.Host := '';
  Result.Port := '';
  if Ch = '/' then
  begin
    Ch := Next(Uri, I);
    if Ch = '/' then
    begin
      Ch := Next(Uri, I);
      Beg := I;
      repeat
        case Ch of
          ':':
          begin
            // Maybe between username and password or host and port.

            // Assume it will become host, because host is read later than username and
            // thus will not overwrite the previously gotten username, if the guess is wrong.
            Result.Host := Copy(Uri, Beg, I - Beg);
            Ch := Next(Uri, I);
            Beg := I;
            repeat
              case Ch of
                '@':
                begin
                  // got username and password
                  Result.Username := Result.Host;
                  Result.Password := Copy(Uri, Beg, I - Beg);
                  Result.Host := '';
                  Ch := Next(Uri, I);
                  Beg := I;
                  break;
                end;

                '/', '?', '#', #0:
                begin
                  // got host and port
                  Result.Port := Copy(Uri, Beg, I - Beg);
                  IsPort := True;
                  Beg := I;
                  break;
                end;

              end;
              Ch := Next(Uri, I);
            until False;
            continue;
          end;

          '[':
            if I = Beg then
            begin
              // ipv6address or ipvFuture
              Ch := Next(Uri, I);
              repeat
                case Ch of
                  ']':
                    break;

                  '/', '?', '#', #0:
                    break;
                end;
                Ch := Next(Uri, I);
              until False;
            end;

          '/', '?', '#', #0:
          begin
            // path begins or hierarchy is omitted
            if not IsPort then
            begin
              // remember host, as not yet done with the port.
              Result.Host := Copy(Uri, Beg, I - Beg);
              Beg := I;
            end;
            break;
          end;

        end;
        Ch := Next(Uri, I);
      until False;
    end;
  end;

  // parse path
  Result.Path := '';
  if Ch = '/' then
  begin
    // skip leading '/'
    Ch := Next(Uri, I);
    Beg := I;
  end;
  repeat
    case Ch of
      '?', '#', #0:
      begin
        // end of path
        Result.Path := Copy(Uri, Beg, I - Beg);
        break;
      end;
    end;
    Ch := Next(Uri, I);
  until False;

  // parse query
  Result.Query := '';
  if Ch = '?' then
  begin
    Ch := Next(Uri, I);
    Beg := I;
    repeat
      case Ch of
        '#', #0:
        begin
          // end of path
          Result.Query := Copy(Uri, Beg, I - Beg);
          break;
        end;
      end;
      Ch := Next(Uri, I);
    until False;
  end;

  // parse fragment
  Result.Fragment := '';
  if Ch = '#' then
  begin
    Beg := I + 1;
    I := Length(Uri) + 1;
    Result.Fragment := Copy(Uri, Beg, I - Beg);
  end;
end;

//-- BG ---------------------------------------------------------- 21.08.2011 --
function UriToStr(Uri: TUri): ThtString;
var
  HierarchyPrefix: ThtString;
begin
  Result := '';
  if Length(Uri.Scheme) > 0 then Result := Uri.Scheme + ':';
  HierarchyPrefix := '//';
  if Length(Uri.Username) > 0 then
  begin
    Result := Result + HierarchyPrefix + Uri.Username + ':' + Uri.Password + '@';
    HierarchyPrefix := '';
  end;
  if Length(Uri.Host) > 0 then
  begin
    Result := Result + HierarchyPrefix + Uri.Host;
    HierarchyPrefix := '/';
  end
  else if (Length(Uri.Path) >= 2) and (Uri.Path[2] = ':') and (htCompareString(Uri.Scheme, 'file') = 0) then
    HierarchyPrefix := '///'
  else
    HierarchyPrefix := '';

  if Length(Uri.Port)     > 0 then Result := Result + ':' + Uri.Port;
  if Length(Uri.Path)     > 0 then Result := Result + HierarchyPrefix + Uri.Path;
  if Length(Uri.Query)    > 0 then Result := Result + '?' + Uri.Query;
  if Length(Uri.Fragment) > 0 then Result := Result + '#' + Uri.Fragment;
end;

//-- BG ---------------------------------------------------------- 27.08.2011 --
function StrToUri(Str, DefaultScheme: ThtString): TUri;
begin
  Result := StrToUri(Str);
  if Result.Scheme = '' then
    Result.Scheme := DefaultScheme;
  NormalizeUri(Result);
end;

//-- BG ---------------------------------------------------------- 27.08.2011 --
function StrToUriStr(Str, DefaultScheme: ThtString): ThtString; overload;
var
  Uri: TUri;
begin
  Uri := StrToUri(Str, DefaultScheme);
  Result := UriToStr(Uri);
end;

end.
