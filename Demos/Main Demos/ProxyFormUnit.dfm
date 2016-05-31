object ProxyForm: TProxyForm
  Left = 298
  Top = 180
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Proxy'
  ClientHeight = 254
  ClientWidth = 287
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 26
    Height = 13
    Caption = 'Proxy'
  end
  object Label2: TLabel
    Left = 216
    Top = 16
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object Label3: TLabel
    Left = 24
    Top = 64
    Width = 77
    Height = 13
    Caption = 'Proxy Username'
  end
  object Label4: TLabel
    Left = 24
    Top = 112
    Width = 75
    Height = 13
    Caption = 'Proxy Password'
  end
  object Label5: TLabel
    Left = 24
    Top = 160
    Width = 53
    Height = 13
    Caption = 'User Agent'
  end
  object ProxyEdit: TEdit
    Left = 24
    Top = 32
    Width = 177
    Height = 21
    TabOrder = 0
  end
  object PortEdit: TEdit
    Left = 216
    Top = 32
    Width = 49
    Height = 21
    TabOrder = 1
    Text = '80'
  end
  object OKButton: TBitBtn
    Left = 24
    Top = 216
    Width = 91
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object CancelButton: TBitBtn
    Left = 176
    Top = 216
    Width = 91
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
  object ProxyUsername: TEdit
    Left = 24
    Top = 80
    Width = 241
    Height = 21
    TabOrder = 4
  end
  object ProxyPassword: TEdit
    Left = 24
    Top = 128
    Width = 241
    Height = 21
    TabOrder = 5
  end
  object UserAgent: TEdit
    Left = 24
    Top = 176
    Width = 241
    Height = 21
    TabOrder = 6
    Text = 'Mozilla/4.0'
  end
end
