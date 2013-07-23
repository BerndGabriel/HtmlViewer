 {
 options.inc


UseSSL - Enables the OpenSourceSSL support (https).

UseZLib - set this to support ZLib and Gzip http compression.

LogIt - set this to support a diagnostic window.
}
{$define UseSSL}

{$define UseZLib}

{$define TSearchRecHasNoTimestamp}

{..$define LogIt}

{To include the Zip: protocol, define "IncludeZip" by removing the "..."}
{...$Define IncludeZip}
