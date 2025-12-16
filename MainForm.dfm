object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'REST API Client with Token Management'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Size = 8
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 784
    Height = 129
    Caption = ' Authentication '
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 28
      Height = 13
      Caption = 'Email:'
    end
    object Label2: TLabel
      Left = 16
      Top = 51
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object Label3: TLabel
      Left = 16
      Top = 102
      Width = 69
      Height = 13
      Caption = 'Token Status:'
    end
    object LabelTokenStatus: TLabel
      Left = 91
      Top = 102
      Width = 116
      Height = 13
      Caption = 'NOT AUTHENTICATED'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Size = 8
      Font.Style = [fsBold]
      ParentFont = False
    end
    object EditEmail: TEdit
      Left = 72
      Top = 21
      Width = 300
      Height = 21
      TabOrder = 0
      Text = 'user@example.com'
    end
    object EditPassword: TEdit
      Left = 72
      Top = 48
      Width = 300
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
      Text = 'password123'
    end
    object btnLogin: TButton
      Left = 16
      Top = 75
      Width = 100
      Height = 25
      Caption = 'Login'
      TabOrder = 2
      OnClick = btnLoginClick
    end
    object btnRefreshToken: TButton
      Left = 122
      Top = 75
      Width = 120
      Height = 25
      Caption = 'Refresh Token'
      TabOrder = 3
      OnClick = btnRefreshTokenClick
    end
    object btnClearTokens: TButton
      Left = 248
      Top = 75
      Width = 100
      Height = 25
      Caption = 'Clear Tokens'
      TabOrder = 4
      OnClick = btnClearTokensClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 143
    Width = 784
    Height = 434
    Caption = ' API Operations '
    TabOrder = 1
    object btnGetUsers: TButton
      Left = 16
      Top = 24
      Width = 120
      Height = 25
      Caption = 'Get Users List'
      TabOrder = 0
      OnClick = btnGetUsersClick
    end
    object MemoResponse: TMemo
      Left = 16
      Top = 55
      Width = 752
      Height = 362
      ScrollBars = ssBoth
      TabOrder = 1
      WordWrap = False
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 581
    Width = 800
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object PanelLoading: TPanel
    Left = 232
    Top = 224
    Width = 337
    Height = 153
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Color = clWhite
    ParentBackground = False
    TabOrder = 3
    Visible = False
    object ShapeLoading: TShape
      Left = 144
      Top = 24
      Width = 49
      Height = 49
      Brush.Color = clBlue
      Shape = stCircle
    end
    object LabelLoading: TLabel
      Left = 16
      Top = 88
      Width = 305
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = 'Please wait...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Size = 10
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ProgressBarLoading: TProgressBar
      Left = 16
      Top = 110
      Width = 305
      Height = 17
      TabOrder = 0
    end
    object btnCancelRequest: TButton
      Left = 232
      Top = 16
      Width = 89
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelRequestClick
    end
  end
  object RESTClient1: TRESTClient
    Accept = 'application/json, text/plain; q=0.9, text/html;q=0.8,'
    AcceptCharset = 'utf-8, *;q=0.8'
    BaseURL = 'https://api.example.com'
    Params = <>
    HandleRedirects = True
    RaiseExceptionOn500 = False
    Left = 408
    Top = 16
  end
  object RESTRequest1: TRESTRequest
    AssignedValues = [rvConnectTimeout, rvReadTimeout]
    Client = RESTClient1
    Params = <>
    Response = RESTResponse1
    SynchronizedEvents = False
    Left = 472
    Top = 16
  end
  object RESTResponse1: TRESTResponse
    ContentType = 'application/json'
    Left = 536
    Top = 16
  end
  object OAuth2Authenticator1: TOAuth2Authenticator
    AccessToken = 'AccessToken'
    TokenType = ttBEARER
    Left = 616
    Top = 16
  end
  object TimerLoading: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerLoadingTimer
    Left = 688
    Top = 16
  end
end