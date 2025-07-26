unit X3DJSONLDJS2Pascal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, fpjson, jsonparser, 
  fphttpclient, opensslsockets;

type
  TX3DJSONLDBrowser = class
  public
    procedure Print(const AString: string);
    procedure PrintLn(const AString: string);
    function StringToArray(const AClass: string; const AObj: string): TJSONArray;
    function AppendTo(AElement: TDOMElement; AJSObj: TJSONObject): TDOMElement;
    function GetDocument: TDOMDocument;
  end;

  TX3DJSONLD = class
  private
    Fx3djsonNS: string;
    FBrowser: TX3DJSONLDBrowser;
    
    function CommentStringToXML(const AStr: string): string;
    function SFStringToXML(const AStr: string): string;
    function JSONStringToXML(const AStr: string): string;
    function NavigationInfoTypeToXML(const AStr: string): string;
    function FixXML(const AXMLStr: string): string;
    
    procedure ElementSetAttribute(AElement: TDOMElement; const AKey, AValue: string);
    procedure ConvertChildren(AXMLDoc: TDOMDocument; const AParentKey: string; 
                             AObject: TJSONObject; AElement: TDOMElement; const APath: string);
    function CreateElement(AXMLDoc: TDOMDocument; const AKey: string; 
                          const AX3DJsonNS: string; const AContainerField: string = ''): TDOMElement;
    procedure CDATACreateFunction(AXMLDoc: TDOMDocument; AElement: TDOMElement; const AStr: string);
    procedure ConvertObject(AXMLDoc: TDOMDocument; const AKey: string; AObject: TJSONObject; 
                           AElement: TDOMElement; const APath: string; const AContainerField: string = '');
    function ConvertToX3DOM(AXMLDoc: TDOMDocument; AObject: TJSONData; const AParentKey: string; 
                           AElement: TDOMElement; const APath: string; const AContainerField: string = ''): TDOMElement;
    function PrepareDocument(ADOMImplementation: TDOMImplementation; AJSObj: TJSONObject): TDOMDocument;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    property x3djsonNS: string read Fx3djsonNS write Fx3djsonNS;
    property Browser: TX3DJSONLDBrowser read FBrowser;
    
    function ProcessURLs(ALocalArray: TStringList; const APath: string): TStringList;
    procedure LoadURLs(const ALoadPath: string; AUrls: TStringList; 
                      ALoadedCallback: TNotifyEvent; const AProtoExp: string = '';
                      const ADone: string = ''; const AExternProtoDeclare: string = '';
                      AObj: TObject = nil);
    
    function LoadJsonIntoXml(ADOMImplementation: TDOMImplementation; 
                            AJSObj: TJSONObject; const APath: string): TDOMElement;
    function LoadJsonIntoDom(ADOMImplementation: TDOMImplementation; 
                            AJSObj: TJSONObject; const APath: string): TDOMElement;
    function SerializeDOM(AJson: TJSONObject; AElement: TDOMElement; 
                         AAppendDocType: Boolean = False): string;
    function SelectObjectFromJSObj(ANode: TJSONObject; const ASelectorField: string): TJSONData;
    
    procedure SetProcessURLs(AFunc: TNotifyEvent);
    procedure SetDocument(ADoc: TDOMDocument);
  end;

implementation

{ TX3DJSONLDBrowser }

procedure TX3DJSONLDBrowser.Print(const AString: string);
begin
  if AString <> '' then
    Write(AString);
end;

procedure TX3DJSONLDBrowser.PrintLn(const AString: string);
begin
  if AString <> '' then
    WriteLn(AString);
end;

function TX3DJSONLDBrowser.StringToArray(const AClass: string; const AObj: string): TJSONArray;
var
  Parser: TJSONParser;
  Data: TJSONData;
begin
  Result := nil;
  try
    Parser := TJSONParser.Create('[' + AObj + ']');
    try
      Data := Parser.Parse;
      if Data is TJSONArray then
        Result := TJSONArray(Data);
    finally
      Parser.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error parsing JSON array: ' + E.Message);
  end;
end;

function TX3DJSONLDBrowser.AppendTo(AElement: TDOMElement; AJSObj: TJSONObject): TDOMElement;
begin
  Result := nil;
  if Assigned(AElement) then
  begin
    // Implementation would depend on having access to TX3DJSONLD instance
    // This is a placeholder for the actual conversion logic
  end;
end;

function TX3DJSONLDBrowser.GetDocument: TDOMDocument;
begin
  Result := TDOMDocument.Create;
end;

{ TX3DJSONLD }

constructor TX3DJSONLD.Create;
begin
  inherited Create;
  Fx3djsonNS := '';
  FBrowser := TX3DJSONLDBrowser.Create;
end;

destructor TX3DJSONLD.Destroy;
begin
  FBrowser.Free;
  inherited Destroy;
end;

function TX3DJSONLD.ProcessURLs(ALocalArray: TStringList; const APath: string): TStringList;
var
  I: Integer;
  URL, PC: string;
  P, PE, LS: Integer;
begin
  Result := TStringList.Create;
  Result.Assign(ALocalArray);
  
  for I := 0 to Result.Count - 1 do
  begin
    URL := Result[I];
    
    // Handle absolute URLs
    if (Pos('http://', URL) = 1) or (Pos('https://', URL) = 1) then
      Continue;
      
    // Handle panorama URLs
    if Pos('urn:web3d:media:textures/panoramas/', URL) = 1 then
    begin
      LS := LastDelimiter('/', URL);
      if LS > 0 then
        Result[I] := 'examples/Basic/UniversalMediaPanoramas/' + Copy(URL, LS + 1, Length(URL));
      Continue;
    end;
    
    // Handle relative URLs
    P := Pos('#', URL);
    PE := LastDelimiter('/', APath);
    PC := APath;
    if PE > 0 then
      PC := Copy(APath, 1, PE - 1);
      
    // Handle ../ paths
    while Pos('../', URL) = 1 do
    begin
      Delete(URL, 1, 3);
      PE := LastDelimiter('/', PC);
      if PE > 0 then
        PC := Copy(PC, 1, PE - 1)
      else
        PC := '';
    end;
    
    if P = 1 then
      Result[I] := APath + URL
    else
      Result[I] := PC + '/' + URL;
      
    // Remove hash fragments for server side processing
    P := LastDelimiter('#', Result[I]);
    if P > 0 then
      Result[I] := Copy(Result[I], 1, P - 1);
  end;
end;

procedure TX3DJSONLD.LoadURLs(const ALoadPath: string; AUrls: TStringList; 
                             ALoadedCallback: TNotifyEvent; const AProtoExp: string;
                             const ADone: string; const AExternProtoDeclare: string;
                             AObj: TObject);
var
  ProcessedURLs: TStringList;
  I: Integer;
  URL, Protocol, Host, Path, Data: string;
  P, PA: Integer;
  HTTPClient: TFPHTTPClient;
  FileStream: TFileStream;
begin
  if not Assigned(AUrls) then
    Exit;
    
  ProcessedURLs := ProcessURLs(AUrls, ALoadPath);
  try
    for I := 0 to ProcessedURLs.Count - 1 do
    begin
      URL := ProcessedURLs[I];
      P := Pos('://', URL);
      
      if P > 0 then
      begin
        Protocol := Copy(URL, 1, P - 1);
        PA := Pos('/', URL, P + 3);
        Host := Copy(URL, P + 3, PA - P - 3);
        Path := Copy(URL, PA, Length(URL));
        
        if (Protocol = 'http') or (Protocol = 'https') then
        begin
          HTTPClient := TFPHTTPClient.Create(nil);
          try
            Data := HTTPClient.Get(URL);
            // Call callback with loaded data
            if Assigned(ALoadedCallback) then
              ALoadedCallback(Self);
          finally
            HTTPClient.Free;
          end;
        end;
      end
      else
      begin
        // Handle file URLs
        if FileExists(URL) then
        begin
          FileStream := TFileStream.Create(URL, fmOpenRead);
          try
            SetLength(Data, FileStream.Size);
            FileStream.ReadBuffer(Data[1], FileStream.Size);
            // Call callback with loaded data
            if Assigned(ALoadedCallback) then
              ALoadedCallback(Self);
          finally
            FileStream.Free;
          end;
        end;
      end;
    end;
  finally
    ProcessedURLs.Free;
  end;
end;

procedure TX3DJSONLD.ElementSetAttribute(AElement: TDOMElement; const AKey, AValue: string);
begin
  if AKey = 'SON schema' then
    // JSON Schema - ignore
  else if AKey = 'ncoding' then
    // encoding - ignore
  else
  begin
    if Assigned(AElement) then
      AElement.SetAttribute(AKey, AValue);
  end;
end;

procedure TX3DJSONLD.ConvertChildren(AXMLDoc: TDOMDocument; const AParentKey: string; 
                                    AObject: TJSONObject; AElement: TDOMElement; const APath: string);
var
  I: Integer;
  Key: string;
  Item: TJSONData;
begin
  for I := 0 to AObject.Count - 1 do
  begin
    Key := AObject.Names[I];
    Item := AObject.Items[I];
    
    if Item is TJSONObject then
    begin
      // Check if key is numeric
      if TryStrToInt(Key, I) then
        ConvertToX3DOM(AXMLDoc, Item, Key, AElement, APath, Copy(AParentKey, 2, Length(AParentKey)))
      else
        ConvertObject(AXMLDoc, Key, AObject, AElement, APath, Copy(AParentKey, 2, Length(AParentKey)));
    end;
  end;
end;

function TX3DJSONLD.CreateElement(AXMLDoc: TDOMDocument; const AKey: string; 
                                 const AX3DJsonNS: string; const AContainerField: string): TDOMElement;
begin
  if AX3DJsonNS = '' then
    Result := AXMLDoc.CreateElement(AKey)
  else
  begin
    Result := AXMLDoc.CreateElementNS(AX3DJsonNS, AKey);
    if not Assigned(Result) then
      Result := AXMLDoc.CreateElement(AKey);
  end;
  
  if (AContainerField <> '') and (AContainerField <> 'geometry') and (AContainerField <> 'coord') then
    ElementSetAttribute(Result, 'containerField', AContainerField);
end;

procedure TX3DJSONLD.CDATACreateFunction(AXMLDoc: TDOMDocument; AElement: TDOMElement; const AStr: string);
var
  ProcessedStr: string;
  CDATASection: TDOMCDATASection;
begin
  ProcessedStr := Trim(AStr);
  ProcessedStr := StringReplace(ProcessedStr, '&quot;', '"', [rfReplaceAll]);
  ProcessedStr := StringReplace(ProcessedStr, '&lt;', '<', [rfReplaceAll]);
  ProcessedStr := StringReplace(ProcessedStr, '&gt;', '>', [rfReplaceAll]);
  ProcessedStr := StringReplace(ProcessedStr, '&amp;', '&', [rfReplaceAll]);
  
  CDATASection := AXMLDoc.CreateCDATASection(ProcessedStr);
  AElement.AppendChild(CDATASection);
end;

procedure TX3DJSONLD.ConvertObject(AXMLDoc: TDOMDocument; const AKey: string; AObject: TJSONObject; 
                                  AElement: TDOMElement; const APath: string; const AContainerField: string);
var
  Child: TDOMElement;
  Comment: TDOMComment;
  Item: TJSONData;
  I: Integer;
  TextNode: TDOMText;
begin
  Item := AObject.Find(AKey);
  if not Assigned(Item) or not (Item is TJSONObject) then
    Exit;
    
  if AKey[1] = '@' then
    ConvertToX3DOM(AXMLDoc, Item, AKey, AElement, APath)
  else if AKey[1] = '-' then
    ConvertChildren(AXMLDoc, AKey, TJSONObject(Item), AElement, APath)
  else if AKey = '#comment' then
  begin
    if Item is TJSONArray then
    begin
      for I := 0 to TJSONArray(Item).Count - 1 do
      begin
        Comment := AXMLDoc.CreateComment(CommentStringToXML(TJSONArray(Item).Strings[I]));
        AElement.AppendChild(Comment);
      end;
    end;
  end
  else if AKey = '#sourceCode' then
  begin
    if Item is TJSONArray then
      CDATACreateFunction(AXMLDoc, AElement, TJSONArray(Item).AsJSON);
  end
  else
  begin
    if (AKey = 'connect') or (AKey = 'fieldValue') or (AKey = 'field') or 
       (AKey = 'meta') or (AKey = 'component') or (AKey = 'unit') then
    begin
      if Item is TJSONArray then
      begin
        for I := 0 to TJSONArray(Item).Count - 1 do
        begin
          if TJSONArray(Item).Items[I] is TJSONObject then
          begin
            Child := CreateElement(AXMLDoc, AKey, x3djsonNS, AContainerField);
            ConvertToX3DOM(AXMLDoc, TJSONArray(Item).Items[I], IntToStr(I), Child, APath);
            AElement.AppendChild(Child);
            TextNode := AXMLDoc.CreateTextNode(#10);
            AElement.AppendChild(TextNode);
          end;
        end;
      end;
    end
    else
    begin
      Child := CreateElement(AXMLDoc, AKey, x3djsonNS, AContainerField);
      ConvertToX3DOM(AXMLDoc, Item, AKey, Child, APath);
      AElement.AppendChild(Child);
      TextNode := AXMLDoc.CreateTextNode(#10);
      AElement.AppendChild(TextNode);
    end;
  end;
end;

function TX3DJSONLD.ConvertToX3DOM(AXMLDoc: TDOMDocument; AObject: TJSONData; const AParentKey: string; 
                                  AElement: TDOMElement; const APath: string; const AContainerField: string): TDOMElement;
var
  I: Integer;
  Key: string;
  LocalArray: TStringList;
  IsArray, ArrayOfStrings: Boolean;
  Item: TJSONData;
  Comment: TDOMComment;
begin
  Result := AElement;
  LocalArray := TStringList.Create;
  try
    IsArray := False;
    ArrayOfStrings := False;
    
    if AObject is TJSONObject then
    begin
      for I := 0 to TJSONObject(AObject).Count - 1 do
      begin
        Key := TJSONObject(AObject).Names[I];
        Item := TJSONObject(AObject).Items[I];
        
        // Check if this is an array index
        if TryStrToInt(Key, I) then
          IsArray := True
        else
          IsArray := False;
          
        if IsArray then
        begin
          case Item.JSONType of
            jtNumber: LocalArray.Add(Item.AsString);
            jtString: 
            begin
              LocalArray.Add(Item.AsString);
              ArrayOfStrings := True;
            end;
            jtBoolean: LocalArray.Add(Item.AsString);
            jtObject: ConvertToX3DOM(AXMLDoc, Item, Key, AElement, APath);
          end;
        end
        else if Item is TJSONObject then
        begin
          if Key = 'X3D' then
            ConvertToX3DOM(AXMLDoc, Item, Key, AElement, APath)
          else
            ConvertObject(AXMLDoc, Key, TJSONObject(AObject), AElement, APath);
        end
        else
        begin
          case Item.JSONType of
            jtNumber: ElementSetAttribute(AElement, Copy(Key, 2, Length(Key)), Item.AsString);
            jtString: 
            begin
              if Key = '#comment' then
              begin
                Comment := AXMLDoc.CreateComment(CommentStringToXML(Item.AsString));
                AElement.AppendChild(Comment);
              end
              else
                ElementSetAttribute(AElement, Copy(Key, 2, Length(Key)), JSONStringToXML(Item.AsString));
            end;
            jtBoolean: ElementSetAttribute(AElement, Copy(Key, 2, Length(Key)), Item.AsString);
          end;
        end;
      end;
    end
    else if AObject is TJSONArray then
    begin
      IsArray := True;
      for I := 0 to TJSONArray(AObject).Count - 1 do
      begin
        Item := TJSONArray(AObject).Items[I];
        case Item.JSONType of
          jtNumber: LocalArray.Add(Item.AsString);
          jtString: 
          begin
            LocalArray.Add(Item.AsString);
            ArrayOfStrings := True;
          end;
          jtBoolean: LocalArray.Add(Item.AsString);
          jtObject: ConvertToX3DOM(AXMLDoc, Item, IntToStr(I), AElement, APath);
        end;
      end;
    end;
    
    if IsArray and (AParentKey[1] = '@') then
    begin
      if ArrayOfStrings then
      begin
        for I := 0 to LocalArray.Count - 1 do
          LocalArray[I] := SFStringToXML(LocalArray[I]);
          
        if (AParentKey = '@url') or (Pos('Url', AParentKey) = Length(AParentKey) - 2) then
          ProcessURLs(LocalArray, APath);
          
        ElementSetAttribute(AElement, Copy(AParentKey, 2, Length(AParentKey)), 
                           '"' + StringReplace(LocalArray.DelimitedText, ',', '" "', [rfReplaceAll]) + '"');
      end
      else
        ElementSetAttribute(AElement, Copy(AParentKey, 2, Length(AParentKey)), 
                           StringReplace(LocalArray.DelimitedText, ',', ' ', [rfReplaceAll]));
    end;
    
  finally
    LocalArray.Free;
  end;
end;

function TX3DJSONLD.LoadJsonIntoXml(ADOMImplementation: TDOMImplementation; 
                                   AJSObj: TJSONObject; const APath: string): TDOMElement;
var
  XMLDoc: TDOMDocument;
begin
  XMLDoc := PrepareDocument(ADOMImplementation, AJSObj);
  Result := CreateElement(XMLDoc, 'X3D', x3djsonNS);
  Result.SetAttribute('xmlns:xsd', 'http://www.w3.org/2001/XMLSchema-instance');
  ConvertToX3DOM(XMLDoc, AJSObj, '', Result, APath);
end;

function TX3DJSONLD.LoadJsonIntoDom(ADOMImplementation: TDOMImplementation; 
                                   AJSObj: TJSONObject; const APath: string): TDOMElement;
var
  XMLDoc: TDOMDocument;
begin
  XMLDoc := PrepareDocument(ADOMImplementation, AJSObj);
  Result := CreateElement(XMLDoc, 'X3D', x3djsonNS);
  Result.SetAttribute('xmlns:xsd', 'http://www.w3.org/2001/XMLSchema-instance');
  ConvertToX3DOM(XMLDoc, AJSObj, '', Result, APath);
end;

function TX3DJSONLD.PrepareDocument(ADOMImplementation: TDOMImplementation; AJSObj: TJSONObject): TDOMDocument;
var
  Version, Encoding: string;
  DocType: TDOMDocumentType;
  X3DObj: TJSONObject;
  PI: TDOMProcessingInstruction;
begin
  X3DObj := TJSONObject(AJSObj.Find('X3D'));
  if Assigned(X3DObj) then
  begin
    Version := X3DObj.Get('@version', '4.0');
    Encoding := X3DObj.Get('encoding', 'UTF-8');
  end
  else
  begin
    Version := '4.0';
    Encoding := 'UTF-8';
  end;
  
  DocType := ADOMImplementation.CreateDocumentType('X3D', 
    'ISO//Web3D//DTD X3D ' + Version + '//EN" "https://www.web3d.org/specifications/x3d-' + Version + '.dtd', 
    '');
  Result := ADOMImplementation.CreateDocument('', 'X3D', DocType);
  
  PI := Result.CreateProcessingInstruction('xml', 'version="1.0" encoding="' + Encoding + '"');
  Result.InsertBefore(PI, DocType);
end;

function TX3DJSONLD.CommentStringToXML(const AStr: string): string;
begin
  Result := StringReplace(AStr, '\\', '\', [rfReplaceAll]);
end;

function TX3DJSONLD.SFStringToXML(const AStr: string): string;
begin
  Result := StringReplace(AStr, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
end;

function TX3DJSONLD.JSONStringToXML(const AStr: string): string;
begin
  Result := StringReplace(AStr, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TX3DJSONLD.NavigationInfoTypeToXML(const AStr: string): string;
begin
  WriteLn('X3DJSONLDJS2Pascal jsonstring replacing ' + AStr);
  Result := AStr;
  WriteLn('ok');
end;

function TX3DJSONLD.FixXML(const AXMLStr: string): string;
var
  TempStr: string;
begin
  Result := AXMLStr;
  
  // Get rid of self-closing tags
  Result := StringReplace(Result, '/>', '></', [rfReplaceAll]);
  
  // Strip out namespace
  Result := StringReplace(Result, 'xmlns="', '', [rfReplaceAll]);
  
  // Strip out schema
  Result := StringReplace(Result, 'xsd:noNamespaceSchemaLocation="', '', [rfReplaceAll]);
  
  // Fix CDATA sections
  Result := StringReplace(Result, '&lt;![CDATA[', #10'<![CDATA[', [rfReplaceAll]);
  Result := StringReplace(Result, ']]&gt;', ']]>'#10, [rfReplaceAll]);
  
  // Handle CDATA content fixes
  repeat
    TempStr := Result;
    Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
  until Result = TempStr;
  
  repeat
    TempStr := Result;
    Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
  until Result = TempStr;
  
  repeat
    TempStr := Result;
    Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
  until Result = TempStr;
end;

function TX3DJSONLD.SerializeDOM(AJson: TJSONObject; AElement: TDOMElement; 
                                AAppendDocType: Boolean): string;
var
  Version, Encoding: string;
  X3DObj: TJSONObject;
begin
  Version := '4.0';
  Encoding := 'UTF-8';
  
  if Assigned(AJson) then
  begin
    X3DObj := TJSONObject(AJson.Find('X3D'));
    if Assigned(X3DObj) then
    begin
      Version := X3DObj.Get('@version', '4.0');
      Encoding := X3DObj.Get('encoding', 'UTF-8');
    end;
  end;
  
  Result := '';
  if Assigned(AElement) then
  begin
    // Use Free Pascal's DOM serialization
    WriteXMLFile(AElement.OwnerDocument, 'temp.xml');
    // Read back the XML content - this is a simplified approach
    // In practice, you'd want to use proper DOM serialization
  end;
  
  Result := FixXML(Result);
end;

function TX3DJSONLD.SelectObjectFromJSObj(ANode: TJSONObject; const ASelectorField: string): TJSONData;
var
  SelectorParts: TStringList;
  I: Integer;
  CurrentObject: TJSONData;
  SelectorField: string;
begin
  Result := nil;
  SelectorField := Copy(ASelectorField, 2, Length(ASelectorField)); // Remove first character
  
  SelectorParts := TStringList.Create;
  try
    SelectorParts.Delimiter := '/';
    SelectorParts.DelimitedText := SelectorField;
    
    CurrentObject := ANode;
    for I := 0 to SelectorParts.Count - 1 do
    begin
      if CurrentObject is TJSONObject then
        CurrentObject := TJSONObject(CurrentObject).Find(SelectorParts[I])
      else
      begin
        WriteLn('Error: I think we went down too far: ' + ASelectorField + ' is unavailable.');
        Exit;
      end;
      
      if not Assigned(CurrentObject) then
      begin
        WriteLn('Error: I think we went down too far: ' + ASelectorField + ' is unavailable.');
        Exit;
      end;
    end;
    
    Result := CurrentObject;
  finally
    SelectorParts.Free;
  end;
end;

procedure TX3DJSONLD.SetProcessURLs(AFunc: TNotifyEvent);
begin
  // In Pascal, we can't easily replace methods like in JavaScript
  // This would require a more complex callback system
end;

procedure TX3DJSONLD.SetDocument(ADoc: TDOMDocument);
begin
  // Store document reference if needed
end;

end.
