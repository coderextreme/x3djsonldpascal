unit X3DJSONLDJava2Pascal;

{
  Copyright (c) 2022. John Carlson
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

  * Neither the name of content nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE
}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, fpjson, jsonparser, 
  Generics.Collections, CastlePasJSON, StrUtils;

type
  TX3DJSONLD = class
  private
    x3dTidy: Boolean;
    //protos: TDictionary<TString, TPasJSONItemObject>;
    builtins: TStringList;
    
    function StripQuotes(const value: String): String;
    procedure InitializeBuiltins;
    function CommentStringToXML(const str: String): String;
    function NavigationInfoTypeToXML(const str: String): String;
    function FixXML(const str: String; const version: String): String;
    
    procedure ElementSetAttribute(element: TDOMElement; const key: String; 
      const values: TStringList; document: TDOMDocument); overload;
    procedure ElementSetAttribute(element: TDOMElement; const key: String; 
      const value: String; document: TDOMDocument); overload;
    
    function CreateElement(document: TDOMDocument; const key: String; 
      const containerField: String; obj: TPasJSONItemObject): TDOMElement;
    
    procedure CDATACreateFunction(document: TDOMDocument; element: TDOMElement; 
      const value: TPasJSONItemArray);
    
    procedure ConvertProperty(document: TDOMDocument; const key: String; 
      obj: TPasJSONItemObject; element: TDOMElement; const containerField: String);
    
    procedure ConvertJsonObject(document: TDOMDocument; obj: TPasJSONItemObject; 
      const parentkey: String; element: TDOMElement; const containerField: String);
    
    procedure ConvertJsonArray(document: TDOMDocument; arr: TPasJSONItemArray; 
      const parentkey: String; element: TDOMElement; const containerField: String);
    
    function ConvertJsonValue(document: TDOMDocument; value: TPasJSONItem; 
      const parentkey: String; element: TDOMElement; const containerField: String): TDOMElement;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    function LoadJsonIntoDocument(jsobj: TPasJSONItemObject; const version: String;
      x3dTidyFlag: Boolean): TDOMDocument;
    
    function ReadJsonFile(const filename: String): TPasJSONItem;
    function GetX3DVersion(jsobj: TPasJSONItemObject): String;
    function SerializeDOM(const x3dVersion: String; document: TDOMDocument): String;
  end;

implementation

constructor TX3DJSONLD.Create;
begin
  inherited Create;
  x3dTidy := False;
  //protos := TDictionary<String, TPasJSONItemObject>.Create;
  builtins := TStringList.Create;
  builtins.Sorted := True;
  builtins.Duplicates := dupIgnore;
  InitializeBuiltins;
end;

destructor TX3DJSONLD.Destroy;
begin
  //protos.Free;
  builtins.Free;
  inherited Destroy;
end;

function TX3DJSONLD.StripQuotes(const value: String): String;
begin
  if (Length(value) >= 2) and (value[1] = '"') and (value[Length(value)] = '"') then
    Result := Copy(value, 2, Length(value) - 2)
  else
    Result := value;
end;

procedure TX3DJSONLD.InitializeBuiltins;
begin
  // X3D abstract node types
  builtins.Add('X3DAppearanceChildNode');
  builtins.Add('X3DAppearanceNode');
  builtins.Add('X3DArrayField');
  builtins.Add('X3DBackgroundNode');
  builtins.Add('X3DBindableNode');
  builtins.Add('X3DChaserNode');
  builtins.Add('X3DChildNode');
  builtins.Add('X3DColorNode');
  builtins.Add('X3DComposableVolumeRenderStyleNode');
  builtins.Add('X3DComposedGeometryNode');
  builtins.Add('X3DCoordinateNode');
  builtins.Add('X3DDamperNode');
  builtins.Add('X3DDragSensorNode');
  builtins.Add('X3DEnvironmentalSensorNode');
  builtins.Add('X3DEnvironmentTextureNode');
  builtins.Add('X3DField');
  builtins.Add('X3DFollowerNode');
  builtins.Add('X3DFontStyleNode');
  builtins.Add('X3DGeometricPropertyNode');
  builtins.Add('X3DGeometryNode');
  builtins.Add('X3DGroupingNode');
  builtins.Add('X3DInfoNode');
  builtins.Add('X3DInterpolatorNode');
  builtins.Add('X3DKeyDeviceSensorNode');
  builtins.Add('X3DLayerNode');
  builtins.Add('X3DLayoutNode');
  builtins.Add('X3DLightNode');
  builtins.Add('X3DMaterialNode');
  builtins.Add('X3DNBodyCollidableNode');
  builtins.Add('X3DNBodyCollisionSpaceNode');
  builtins.Add('X3DNetworkSensorNode');
  builtins.Add('X3DNode');
  builtins.Add('X3DNormalNode');
  builtins.Add('X3DNurbsControlCurveNode');
  builtins.Add('X3DNurbsSurfaceGeometryNode');
  builtins.Add('X3DOneSidedMaterialNode');
  builtins.Add('X3DParametricGeometryNode');
  builtins.Add('X3DParticleEmitterNode');
  builtins.Add('X3DParticlePhysicsModelNode');
  builtins.Add('X3DPickSensorNode');
  builtins.Add('X3DPointingDeviceSensorNode');
  builtins.Add('X3DProductStructureChildNode');
  builtins.Add('X3DPrototypeInstance');
  builtins.Add('X3DRigidJointNode');
  builtins.Add('X3DScriptNode');
  builtins.Add('X3DSensorNode');
  builtins.Add('X3DSequencerNode');
  builtins.Add('X3DShaderNode');
  builtins.Add('X3DShapeNode');
  builtins.Add('X3DSingleTextureCoordinateNode');
  builtins.Add('X3DSingleTextureNode');
  builtins.Add('X3DSingleTextureTransformNode');
  builtins.Add('X3DSoundChannelNode');
  builtins.Add('X3DSoundDestinationNode');
  builtins.Add('X3DSoundNode');
  builtins.Add('X3DSoundProcessingNode');
  builtins.Add('X3DSoundSourceNode');
  builtins.Add('X3DStatement');
  builtins.Add('X3DTexture2DNode');
  builtins.Add('X3DTexture3DNode');
  builtins.Add('X3DTextureCoordinateNode');
  builtins.Add('X3DTextureNode');
  builtins.Add('X3DTextureProjectorNode');
  builtins.Add('X3DTextureTransformNode');
  builtins.Add('X3DTimeDependentNode');
  builtins.Add('X3DTouchSensorNode');
  builtins.Add('X3DTriggerNode');
  builtins.Add('X3DVertexAttributeNode');
  builtins.Add('X3DViewpointNode');
  builtins.Add('X3DViewportNode');
  builtins.Add('X3DVolumeDataNode');
  builtins.Add('X3DVolumeRenderStyleNode');
  builtins.Add('X3DMaterialExtensionNode');
  builtins.Add('X3DBoundedObject');
  builtins.Add('X3DFogObject');
  builtins.Add('X3DMetadataObject');
  builtins.Add('X3DPickableObject');
  builtins.Add('X3DProgrammableShaderObject');
  builtins.Add('X3DUrlObject');
  
  // X3D concrete node types
  builtins.Add('AcousticProperties');
  builtins.Add('Analyser');
  builtins.Add('Anchor');
  builtins.Add('Appearance');
  builtins.Add('Arc2D');
  builtins.Add('ArcClose2D');
  builtins.Add('AudioClip');
  builtins.Add('AudioDestination');
  builtins.Add('Background');
  builtins.Add('BallJoint');
  builtins.Add('Billboard');
  builtins.Add('BiquadFilter');
  builtins.Add('BlendedVolumeStyle');
  builtins.Add('BooleanFilter');
  builtins.Add('BooleanSequencer');
  builtins.Add('BooleanToggle');
  builtins.Add('BooleanTrigger');
  builtins.Add('BoundaryEnhancementVolumeStyle');
  builtins.Add('BoundedPhysicsModel');
  builtins.Add('Box');
  builtins.Add('BufferAudioSource');
  builtins.Add('CADAssembly');
  builtins.Add('CADFace');
  builtins.Add('CADLayer');
  builtins.Add('CADPart');
  builtins.Add('CartoonVolumeStyle');
  builtins.Add('ChannelMerger');
  builtins.Add('ChannelSelector');
  builtins.Add('ChannelSplitter');
  builtins.Add('Circle2D');
  builtins.Add('ClipPlane');
  builtins.Add('CollidableOffset');
  builtins.Add('CollidableShape');
  builtins.Add('Collision');
  builtins.Add('CollisionCollection');
  builtins.Add('CollisionSensor');
  builtins.Add('CollisionSpace');
  builtins.Add('Color');
  builtins.Add('ColorChaser');
  builtins.Add('ColorDamper');
  builtins.Add('ColorInterpolator');
  builtins.Add('ColorRGBA');
  builtins.Add('ComposedCubeMapTexture');
  builtins.Add('ComposedShader');
  builtins.Add('ComposedTexture3D');
  builtins.Add('ComposedVolumeStyle');
  builtins.Add('Cone');
  builtins.Add('ConeEmitter');
  builtins.Add('Contact');
  builtins.Add('Contour2D');
  builtins.Add('ContourPolyline2D');
  builtins.Add('Convolver');
  builtins.Add('Coordinate');
  builtins.Add('CoordinateChaser');
  builtins.Add('CoordinateDamper');
  builtins.Add('CoordinateDouble');
  builtins.Add('CoordinateInterpolator');
  builtins.Add('CoordinateInterpolator2D');
  builtins.Add('Cylinder');
  builtins.Add('CylinderSensor');
  builtins.Add('Delay');
  builtins.Add('DirectionalLight');
  builtins.Add('DISEntityManager');
  builtins.Add('DISEntityTypeMapping');
  builtins.Add('Disk2D');
  builtins.Add('DoubleAxisHingeJoint');
  builtins.Add('DynamicsCompressor');
  builtins.Add('EaseInEaseOut');
  builtins.Add('EdgeEnhancementVolumeStyle');
  builtins.Add('ElevationGrid');
  builtins.Add('EspduTransform');
  builtins.Add('ExplosionEmitter');
  builtins.Add('Extrusion');
  builtins.Add('FillProperties');
  builtins.Add('FloatVertexAttribute');
  builtins.Add('Fog');
  builtins.Add('FogCoordinate');
  builtins.Add('FontStyle');
  builtins.Add('ForcePhysicsModel');
  builtins.Add('Gain');
  builtins.Add('GeneratedCubeMapTexture');
  builtins.Add('GeoCoordinate');
  builtins.Add('GeoElevationGrid');
  builtins.Add('GeoLocation');
  builtins.Add('GeoLOD');
  builtins.Add('GeoMetadata');
  builtins.Add('GeoOrigin');
  builtins.Add('GeoPositionInterpolator');
  builtins.Add('GeoProximitySensor');
  builtins.Add('GeoTouchSensor');
  builtins.Add('GeoTransform');
  builtins.Add('GeoViewpoint');
  builtins.Add('Group');
  builtins.Add('HAnimDisplacer');
  builtins.Add('HAnimHumanoid');
  builtins.Add('HAnimJoint');
  builtins.Add('HAnimMotion');
  builtins.Add('HAnimSegment');
  builtins.Add('HAnimSite');
  builtins.Add('ImageCubeMapTexture');
  builtins.Add('ImageTexture');
  builtins.Add('ImageTexture3D');
  builtins.Add('IndexedFaceSet');
  builtins.Add('IndexedLineSet');
  builtins.Add('IndexedQuadSet');
  builtins.Add('IndexedTriangleFanSet');
  builtins.Add('IndexedTriangleSet');
  builtins.Add('IndexedTriangleStripSet');
  builtins.Add('Inline');
  builtins.Add('IntegerSequencer');
  builtins.Add('IntegerTrigger');
  builtins.Add('IsoSurfaceVolumeData');
  builtins.Add('KeySensor');
  builtins.Add('Layer');
  builtins.Add('LayerSet');
  builtins.Add('Layout');
  builtins.Add('LayoutGroup');
  builtins.Add('LayoutLayer');
  builtins.Add('LinePickSensor');
  builtins.Add('LineProperties');
  builtins.Add('LineSet');
  builtins.Add('ListenerPointSource');
  builtins.Add('LoadSensor');
  builtins.Add('LocalFog');
  builtins.Add('LOD');
  builtins.Add('Material');
  builtins.Add('Matrix3VertexAttribute');
  builtins.Add('Matrix4VertexAttribute');
  builtins.Add('MetadataBoolean');
  builtins.Add('MetadataDouble');
  builtins.Add('MetadataFloat');
  builtins.Add('MetadataInteger');
  builtins.Add('MetadataSet');
  builtins.Add('MetadataString');
  builtins.Add('MicrophoneSource');
  builtins.Add('MotorJoint');
  builtins.Add('MovieTexture');
  builtins.Add('MultiTexture');
  builtins.Add('MultiTextureCoordinate');
  builtins.Add('MultiTextureTransform');
  builtins.Add('NavigationInfo');
  builtins.Add('Normal');
  builtins.Add('NormalInterpolator');
  builtins.Add('NurbsCurve');
  builtins.Add('NurbsCurve2D');
  builtins.Add('NurbsOrientationInterpolator');
  builtins.Add('NurbsPatchSurface');
  builtins.Add('NurbsPositionInterpolator');
  builtins.Add('NurbsSet');
  builtins.Add('NurbsSurfaceInterpolator');
  builtins.Add('NurbsSweptSurface');
  builtins.Add('NurbsSwungSurface');
  builtins.Add('NurbsTextureCoordinate');
  builtins.Add('NurbsTrimmedSurface');
  builtins.Add('OpacityMapVolumeStyle');
  builtins.Add('OrientationChaser');
  builtins.Add('OrientationDamper');
  builtins.Add('OrientationInterpolator');
  builtins.Add('OrthoViewpoint');
  builtins.Add('OscillatorSource');
  builtins.Add('PackagedShader');
  builtins.Add('ParticleSystem');
  builtins.Add('PeriodicWave');
  builtins.Add('PhysicalMaterial');
  builtins.Add('PickableGroup');
  builtins.Add('PixelTexture');
  builtins.Add('PixelTexture3D');
  builtins.Add('PlaneSensor');
  builtins.Add('PointEmitter');
  builtins.Add('PointLight');
  builtins.Add('PointPickSensor');
  builtins.Add('PointProperties');
  builtins.Add('PointSet');
  builtins.Add('Polyline2D');
  builtins.Add('PolylineEmitter');
  builtins.Add('Polypoint2D');
  builtins.Add('PositionChaser');
  builtins.Add('PositionChaser2D');
  builtins.Add('PositionDamper');
  builtins.Add('PositionDamper2D');
  builtins.Add('PositionInterpolator');
  builtins.Add('PositionInterpolator2D');
  builtins.Add('PrimitivePickSensor');
  builtins.Add('ProgramShader');
  builtins.Add('ProjectionVolumeStyle');
  builtins.Add('ProtoInstance');
  builtins.Add('ProximitySensor');
  builtins.Add('QuadSet');
  builtins.Add('ReceiverPdu');
  builtins.Add('Rectangle2D');
  builtins.Add('RigidBody');
  builtins.Add('RigidBodyCollection');
  builtins.Add('ScalarChaser');
  builtins.Add('ScalarDamper');
  builtins.Add('ScalarInterpolator');
  builtins.Add('ScreenFontStyle');
  builtins.Add('ScreenGroup');
  builtins.Add('Script');
  builtins.Add('SegmentedVolumeData');
  builtins.Add('ShadedVolumeStyle');
  builtins.Add('ShaderPart');
  builtins.Add('ShaderProgram');
  builtins.Add('Shape');
  builtins.Add('SignalPdu');
  builtins.Add('SilhouetteEnhancementVolumeStyle');
  builtins.Add('SingleAxisHingeJoint');
  builtins.Add('SliderJoint');
  builtins.Add('Sound');
  builtins.Add('SpatialSound');
  builtins.Add('Sphere');
  builtins.Add('SphereSensor');
  builtins.Add('SplinePositionInterpolator');
  builtins.Add('SplinePositionInterpolator2D');
  builtins.Add('SplineScalarInterpolator');
  builtins.Add('SpotLight');
  builtins.Add('SquadOrientationInterpolator');
  builtins.Add('StaticGroup');
  builtins.Add('StreamAudioDestination');
  builtins.Add('StreamAudioSource');
  builtins.Add('StringSensor');
  builtins.Add('SurfaceEmitter');
  builtins.Add('Switch');
  builtins.Add('TexCoordChaser2D');
  builtins.Add('TexCoordDamper2D');
  builtins.Add('Text');
  builtins.Add('TextureBackground');
  builtins.Add('TextureCoordinate');
  builtins.Add('TextureCoordinate3D');
  builtins.Add('TextureCoordinate4D');
  builtins.Add('TextureCoordinateGenerator');
  builtins.Add('TextureProjector');
  builtins.Add('TextureProjectorParallel');
  builtins.Add('TextureProperties');
  builtins.Add('TextureTransform');
  builtins.Add('TextureTransform3D');
  builtins.Add('TextureTransformMatrix3D');
  builtins.Add('TimeSensor');
  builtins.Add('TimeTrigger');
  builtins.Add('ToneMappedVolumeStyle');
  builtins.Add('TouchSensor');
  builtins.Add('Transform');
  builtins.Add('TransformSensor');
  builtins.Add('TransmitterPdu');
  builtins.Add('TriangleFanSet');
  builtins.Add('TriangleSet');
  builtins.Add('TriangleSet2D');
  builtins.Add('TriangleStripSet');
  builtins.Add('TwoSidedMaterial');
  builtins.Add('UniversalJoint');
  builtins.Add('UnlitMaterial');
  builtins.Add('Viewpoint');
  builtins.Add('ViewpointGroup');
  builtins.Add('Viewport');
  builtins.Add('VisibilitySensor');
  builtins.Add('VolumeData');
  builtins.Add('VolumeEmitter');
  builtins.Add('VolumePickSensor');
  builtins.Add('WaveShaper');
  builtins.Add('WindPhysicsModel');
  builtins.Add('WorldInfo');
  builtins.Add('EnvironmentLight');
  builtins.Add('Tangent');
  builtins.Add('ImageTextureAtlas');
  builtins.Add('AnisotropyMaterialExtension');
  builtins.Add('BlendMode');
  builtins.Add('ClearcoatMaterialExtension');
  builtins.Add('DepthMode');
  builtins.Add('DispersionMaterialExtension');
  builtins.Add('EmissiveStrengthMaterialExtension');
  builtins.Add('IORMaterialExtension');
  builtins.Add('InstancedShape');
  builtins.Add('IridescenceMaterialExtension');
  builtins.Add('SheenMaterialExtension');
  builtins.Add('SpecularGlossinessMaterial');
  builtins.Add('SpecularMaterialExtension');
  builtins.Add('TransmissionMaterialExtension');
  builtins.Add('VolumeMaterialExtension');
  builtins.Add('DiffuseTransmissionMaterialExtension');
  
  // X3D statements
  builtins.Add('component');
  builtins.Add('connect');
  builtins.Add('EXPORT');
  builtins.Add('ExternProtoDeclare');
  builtins.Add('field');
  builtins.Add('fieldValue');
  builtins.Add('head');
  builtins.Add('IMPORT');
  builtins.Add('IS');
  builtins.Add('meta');
  builtins.Add('ProtoBody');
  builtins.Add('ProtoDeclare');
  builtins.Add('ProtoInterface');
  builtins.Add('ROUTE');
  builtins.Add('Scene');
  builtins.Add('unit');
  builtins.Add('X3D');
  
  // X3D field types
  builtins.Add('SFBool');
  builtins.Add('MFBool');
  builtins.Add('SFColor');
  builtins.Add('MFColor');
  builtins.Add('SFColorRGBA');
  builtins.Add('MFColorRGBA');
  builtins.Add('SFDouble');
  builtins.Add('MFDouble');
  builtins.Add('SFFloat');
  builtins.Add('MFFloat');
  builtins.Add('SFImage');
  builtins.Add('MFImage');
  builtins.Add('SFInt32');
  builtins.Add('MFInt32');
  builtins.Add('SFMatrix3d');
  builtins.Add('MFMatrix3d');
  builtins.Add('SFMatrix3f');
  builtins.Add('MFMatrix3f');
  builtins.Add('SFMatrix4d');
  builtins.Add('MFMatrix4d');
  builtins.Add('SFMatrix4f');
  builtins.Add('MFMatrix4f');
  builtins.Add('SFNode');
  builtins.Add('MFNode');
  builtins.Add('SFRotation');
  builtins.Add('MFRotation');
  builtins.Add('SFString');
  builtins.Add('MFString');
  builtins.Add('SFTime');
  builtins.Add('MFTime');
  builtins.Add('SFVec2d');
  builtins.Add('MFVec2d');
  builtins.Add('SFVec2f');
  builtins.Add('MFVec2f');
  builtins.Add('SFVec3d');
  builtins.Add('MFVec3d');
  builtins.Add('SFVec3f');
  builtins.Add('MFVec3f');
  builtins.Add('SFVec4d');
  builtins.Add('MFVec4d');
  builtins.Add('SFVec4f');
  builtins.Add('MFVec4f');
end;

procedure TX3DJSONLD.ElementSetAttribute(element: TDOMElement; const key: String; 
  const values: TStringList; document: TDOMDocument);
var
  sb: String;
  i: Integer;
  fieldValue: TDOMElement;
begin
  sb := '';
  for i := 0 to values.Count - 1 do
  begin
    if i > 0 then
      sb := sb + ' ';
    sb := sb + values[i];
  end;
  
  if key = 'name' then
    element.SetAttribute(key, sb)
  else if (element.TagName = 'ProtoInstance') and (key <> 'DEF') and (key <> 'name') then
  begin
    fieldValue := document.CreateElement('fieldValue');
    fieldValue.SetAttribute('name', key);
    fieldValue.SetAttribute('value', sb);
    element.AppendChild(fieldValue);
  end
  else
    element.SetAttribute(key, sb);
end;

procedure TX3DJSONLD.ElementSetAttribute(element: TDOMElement; const key: String; 
  const value: String; document: TDOMDocument);
var
  fieldValue: TDOMElement;
begin
  if (element.TagName = 'ProtoInstance') and (key <> 'DEF') and (key <> 'name') then
  begin
    fieldValue := document.CreateElement('fieldValue');
    fieldValue.SetAttribute('name', key);
    fieldValue.SetAttribute('value', value);
    element.AppendChild(fieldValue);
  end
  else if key = 'SON schema' then
    // JSON Schema - ignore
  else if key = 'ncoding' then
    // encoding, UTF-8 - ignore
  else if value = '' then
    element.SetAttribute(key, '')
  else
    element.SetAttribute(key, StripQuotes(value));
end;

function TX3DJSONLD.CreateElement(document: TDOMDocument; const key: String; 
  const containerField: String; obj: TPasJSONItemObject): TDOMElement;
var
  new_object: TPasJSONItemObject;  // used with protos
  child: TDOMElement;
  new_key: String;  // used with protos
  name, DEF: String;
begin
  if (key = 'ProtoDeclare') or (key = 'ExternProtoDeclare') then
  begin
    if Assigned(obj) and Assigned(obj.Properties['@name']) then
    begin
      name := StripQuotes(TPasJSON.getString(obj.Properties['@name'], ''));
      if name <> '' then
      begin
        if builtins.IndexOf(name) >= 0 then
          WriteLn(StdErr, 'Attempt to override builtin name ''', name, ''' rejected')
        //else if protos.ContainsKey(name) then
        //  WriteLn(StdErr, 'Attempt to override PROTO name ''', name, ''' rejected')
        else
        begin
          WriteLn(StdErr, 'PROTO name ', name);
          //protos.Add(name, obj);
        end;
      end;
    end;
    
    if Assigned(obj) and Assigned(obj.Properties['@DEF']) then
    begin
      DEF := StripQuotes(TPasJSON.getString(obj.Properties['@DEF']));
      if DEF <> '' then
      begin
        if builtins.IndexOf(DEF) >= 0 then
          WriteLn(StdErr, 'Attempt to override builtin name ''', DEF, ''' rejected')
        //else if protos.ContainsKey(DEF) then
        //  WriteLn(StdErr, 'Attempt to override PROTO DEF ''', DEF, ''' rejected')
        else
        begin
          WriteLn(StdErr, 'PROTO DEF ', DEF);
          //protos.Add(DEF, obj);
        end;
      end;
    end;
  end;
  
  //if protos.TryGetValue(key, new_object) then
  //begin
  //  new_key := 'ProtoInstance';
  //  child := document.CreateElement(new_key);
  //  child.SetAttribute('name', key);
  //end
  //else
    child := document.CreateElement(key);
  
  if (Length(containerField) > 0) and (containerField <> '') and (
    ((containerField = 'geometry') and (key = 'IndexedFaceSet')) or
    ((containerField = 'geometry') and (key = 'Text')) or
    ((containerField = 'geometry') and (key = 'IndexedTriangleSet')) or
    ((containerField = 'geometry') and (key = 'Sphere')) or
    ((containerField = 'geometry') and (key = 'Cylinder')) or
    ((containerField = 'geometry') and (key = 'Cone')) or
    ((containerField = 'geometry') and (key = 'LineSet')) or
    ((containerField = 'geometry') and (key = 'IndexedLineSet')) or
    ((containerField = 'geometry') and (key = 'Box')) or
    ((containerField = 'geometry') and (key = 'Extrusion')) or
    ((containerField = 'geometry') and (key = 'GeoElevationGrid')) or
    ((containerField = 'shape') and (key = 'Shape')) or
    ((containerField = 'skin') and (key = 'Shape')) or
    (EndsText('exture', containerField) and (key = 'ImageTexture')) or
    (key = 'HAnimSegment') or
    (key = 'HAnimSite') or
    (key = 'HAnimMotion') or
    ((containerField = 'skinCoord') and (key = 'Coordinate')) or
    ((containerField = 'skin') and (key = 'IndexedFaceSet')) or
    (((containerField = 'skinBindingCoords') or (containerField = 'skinCoord')) and (key = 'Coordinate')) or
    (((containerField = 'normal') or (containerField = 'skinBindingNormals') or (containerField = 'skinNormal')) and (key = 'Normal')) or
    (((containerField = 'skeleton') or (containerField = 'children') or (containerField = 'joints')) and (key = 'HAnimJoint'))
  ) then
    child.SetAttribute('containerField', containerField);
  
  Result := child;
end;

procedure TX3DJSONLD.CDATACreateFunction(document: TDOMDocument; element: TDOMElement; 
  const value: TPasJSONItemArray);
var
  sb: String;
  i: Integer;
  str: String;
  cdata: TDOMCDATASection;
begin
  sb := '';
  for i := 0 to value.Count - 1 do
  begin
    if i > 0 then
      sb := sb + #10;
    str := TPasJSON.getString(value.Items[i]);
    str := StringReplace(str, '^"', '', []);
    str := StringReplace(str, '\t', #9, [rfReplaceAll]);
    str := StringReplace(str, '&lt;', '<', [rfReplaceAll]);
    str := StringReplace(str, '&gt;', '>', [rfReplaceAll]);
    str := StringReplace(str, '&amp;', '&', [rfReplaceAll]);
    str := StringReplace(str, '&quot;', '"', [rfReplaceAll]);
    sb := sb + str;
  end;
  
  cdata := document.CreateCDATASection(sb);
  element.AppendChild(cdata);
end;

procedure TX3DJSONLD.ConvertProperty(document: TDOMDocument; const key: String; 
  obj: TPasJSONItemObject; element: TDOMElement; const containerField: String);
var
  jsonValue: TPasJSONItem;
  arr: TPasJSONItemArray;
  i: Integer;
  comment: TDOMComment;
begin
  if not Assigned(obj) then Exit;
  
  jsonValue := obj.Properties[key];
  if not Assigned(jsonValue) then Exit;
  
  if jsonValue is TPasJSONItemObject then
  begin
    if key = '@sourceCode' then
      CDATACreateFunction(document, element, TPasJSONItemArray(jsonValue))
    else if (Length(key) > 0) and (key[1] = '@') then
      ConvertJsonValue(document, jsonValue, key, element, containerField)
    else if (Length(key) > 0) and (key[1] = '-') then
      ConvertJsonValue(document, jsonValue, key, element, Copy(key, 2, Length(key)-1))
    else if key = '#comment' then
    begin
      if jsonValue is TPasJSONItemArray then
      begin
        arr := TPasJSONItemArray(jsonValue);
        for i := 0 to arr.Count - 1 do
        begin
          comment := document.CreateComment(CommentStringToXML(TPasJSON.getString(arr.Items[i])));
          element.AppendChild(comment);
        end;
      end
      else
      begin
        comment := document.CreateComment(CommentStringToXML(TPasJSON.getString(jsonValue)));
        element.AppendChild(comment);
      end;
    end
    else if key = '#sourceCode' then
      CDATACreateFunction(document, element, TPasJSONItemArray(jsonValue))
    else if (key = 'connect') or (key = 'fieldValue') or (key = 'field') or 
            (key = 'meta') or (key = 'component') or (key = 'unit') then
    begin
      arr := TPasJSONItemArray(jsonValue);
      ConvertJsonArray(document, arr, key, element, containerField);
    end
    else
      ConvertJsonValue(document, jsonValue, key, element, containerField);
  end;
end;

function TX3DJSONLD.CommentStringToXML(const str: String): String;
var
  x, y: String;
begin
  y := str;
  Result := y;
  
  repeat
    x := Result;
    Result := StringReplace(x, '\"', '"', [rfReplaceAll]);
  until x = Result;
  
  repeat
    x := Result;
    Result := StringReplace(x, '""', '" "', [rfReplaceAll]);
  until x = Result;
end;

function TX3DJSONLD.NavigationInfoTypeToXML(const str: String): String;
begin
  WriteLn(StdErr, 'X3DJSONLDJava2Pascal jsonstring replacing ', str);
  Result := StringReplace(str, '\', '', [rfReplaceAll]);
  if str <> Result then
    WriteLn(StdErr, 'with                           ', Result)
  else
    WriteLn(StdErr, 'ok');
end;

function TX3DJSONLD.FixXML(const str: String; const version: String): String;
begin
  Result := str;
  // Additional XML fixes could be added here if needed
end;

procedure TX3DJSONLD.ConvertJsonObject(document: TDOMDocument; obj: TPasJSONItemObject; 
  const parentkey: String; element: TDOMElement; const containerField: String);
var
  kii: Boolean;
  child: TDOMElement;
  i: Integer;
  key: String;
  jsonValue: TPasJSONItem;
  tempContainerField: String;
  comment: TDOMComment;
begin
  if not Assigned(obj) then Exit;
  
  // Check if parentkey is numeric
  try
    StrToInt(parentkey);
    kii := True;
  except
    kii := False;
  end;
  
  if kii or (Length(parentkey) > 0) and (parentkey[1] = '-') then
    child := element
  else
  begin
    tempContainerField := containerField;
    
    if ((tempContainerField = '') or (tempContainerField = 'children')) and 
       (parentkey = 'HAnimJoint') and (element.TagName = 'HAnimHumanoid') then
      tempContainerField := 'joints';
      
    if ((tempContainerField = '') or (tempContainerField = 'coord')) and 
       (parentkey = 'Coordinate') and (element.TagName = 'HAnimHumanoid') then
      tempContainerField := 'skinCoord';
      
    child := CreateElement(document, parentkey, tempContainerField, obj);
  end;
  
  for i := 0 to obj.Count - 1 do
  begin
    key := obj.Keys[i];
    jsonValue := obj.Values[i];
    
    if jsonValue is TPasJSONItemObject then
    begin
      if (key = '@type') and (parentkey = 'NavigationInfo') then
        ElementSetAttribute(child, Copy(key, 2, Length(key)-1), 
          NavigationInfoTypeToXML(TPasJSON.getString(jsonValue)), document)
      else if (Length(key) > 0) and (key[1] = '@') then
        ConvertProperty(document, key, TPasJSONItemObject(jsonValue), child, containerField)
      else if (Length(key) > 0) and (key[1] = '-') then
        ConvertJsonObject(document, TPasJSONItemObject(jsonValue), key, child, Copy(key, 2, Length(key)-1))
      else
        ConvertJsonObject(document, TPasJSONItemObject(jsonValue), key, child, containerField);
    end
    else if jsonValue is TPasJSONItemArray then
      ConvertJsonArray(document, TPasJSONItemArray(jsonValue), key, child, containerField)
    else if jsonValue is TPasJSONItemNumber then
      ElementSetAttribute(child, Copy(key, 2, Length(key)-1), TPasJSON.GetString(jsonValue), document)
    else if jsonValue is TPasJSONItemString then
    begin
      if key = '#comment' then
      begin
        comment := document.CreateComment(CommentStringToXML(TPasJSON.GetString(jsonValue)));
        child.AppendChild(comment);
      end
      else if (key = '@type') and (parentkey = 'NavigationInfo') then
        ElementSetAttribute(child, Copy(key, 2, Length(key)-1), 
          NavigationInfoTypeToXML(TPasJSON.GetString(jsonValue)), document)
      else
        ElementSetAttribute(child, Copy(key, 2, Length(key)-1), TPasJSON.GetString(jsonValue), document);
    end
    else if (jsonValue is TPasJSONItemBoolean) or (jsonValue is TPasJSONItemNull) then
      ElementSetAttribute(child, Copy(key, 2, Length(key)-1), TPasJSON.GetString(jsonValue), document);
  end;
  
  if not kii and not ((Length(parentkey) > 0) and (parentkey[1] = '-')) then
    element.AppendChild(child);
end;

procedure TX3DJSONLD.ConvertJsonArray(document: TDOMDocument; arr: TPasJSONItemArray; 
  const parentkey: String; element: TDOMElement; const containerField: String);
var
  arrayOfStrings: Boolean;
  localArray: TStringList;
  arraysize, i: Integer;
  jsonValue: TPasJSONItem;
  kii: Boolean;
begin
  if not Assigned(arr) then Exit;
  
  arrayOfStrings := False;
  localArray := TStringList.Create;
  try
    arraysize := arr.Count;
    
    if parentkey = 'meta' then
    begin
      if (x3dTidy) then
      begin
        arraysize := arr.Count - 2;
      end
      else
      begin
        arraysize := arr.Count - 3;
      end
    end;
    
    for i := 0 to arraysize - 1 do
    begin
      jsonValue := arr.Items[i];
      
      if jsonValue is TPasJSONItemNumber then
        localArray.Add(TPasJSON.GetString(jsonValue))
      else if jsonValue is TPasJSONItemString then
      begin
        localArray.Add(TPasJSON.GetString(jsonValue));
        arrayOfStrings := True;
      end
      else if (jsonValue is TPasJSONItemBoolean) or (jsonValue is TPasJSONItemNull) then
        localArray.Add(TPasJSON.GetString(jsonValue))
      else if jsonValue is TPasJSONItemObject then
      begin
        try
          StrToInt(IntToStr(i));
          kii := True;
        except
          kii := False;
        end;
        
        if not ((Length(parentkey) > 0) and (parentkey[1] = '-')) and kii then
          ConvertJsonValue(document, jsonValue, parentkey, element, containerField)
        else
          ConvertJsonValue(document, jsonValue, IntToStr(i), element, Copy(parentkey, 2, Length(parentkey)-1));
      end
      else if jsonValue is TPasJSONItemArray then
        ConvertJsonValue(document, jsonValue, IntToStr(i), element, containerField);
    end;
    
    if parentkey = '@sourceCode' then
      CDATACreateFunction(document, element, arr)
    else if (Length(parentkey) > 0) and (parentkey[1] = '@') then
      ElementSetAttribute(element, Copy(parentkey, 2, Length(parentkey)-1), localArray, document)
    else if parentkey = '#sourceCode' then
      CDATACreateFunction(document, element, arr);
      
  finally
    localArray.Free;
  end;
end;

function TX3DJSONLD.ConvertJsonValue(document: TDOMDocument; value: TPasJSONItem; 
  const parentkey: String; element: TDOMElement; const containerField: String): TDOMElement;
var
  comment: TDOMComment;
begin
  if value is TPasJSONItemArray then
    ConvertJsonArray(document, TPasJSONItemArray(value), parentkey, element, containerField)
  else
    ConvertJsonObject(document, TPasJSONItemObject(value), parentkey, element, containerField);
  
  Result := element;
end;

function TX3DJSONLD.LoadJsonIntoDocument(jsobj: TPasJSONItemObject; const version: String;
  x3dTidyFlag: Boolean): TDOMDocument;
var
  unenversion: String;
  element: TDOMElement;
  doctype: TDOMDocumentType;
  x3dObj: TPasJSONItemObject;
begin
  x3dTidy := x3dTidyFlag;
  unenversion := StringReplace(StringReplace(version, '%22', '', [rfReplaceAll]), '"', '', [rfReplaceAll]);
  
  Result := TDOMDocument.Create;
  
  element := CreateElement(Result, 'X3D', '', nil);
  ElementSetAttribute(element, 'xmlns:xsd', 'http://www.w3.org/2001/XMLSchema-instance', Result);
  
  x3dObj := TPasJSONItemObject(jsobj.Properties['X3D']);
  if Assigned(x3dObj) then
    ConvertJsonObject(Result, x3dObj, '-', element, '');
  
  Result.AppendChild(element);
  
  // Create DOCTYPE - note: Free Pascal's DOM doesn't support creating doctypes the same way
  // This would need to be handled differently in a real implementation
end;

function TX3DJSONLD.ReadJsonFile(const filename: String): TPasJSONItem;
var
  fileStream: TFileStream;
  parser: TJSONParser;
begin
  fileStream := TFileStream.Create(filename, fmOpenRead);
  try
      Result := TPasJSON.Parse(fileStream);
  finally
    fileStream.Free;
  end;
end;

function TX3DJSONLD.GetX3DVersion(jsobj: TPasJSONItemObject): String;
var
  x3dObj: TPasJSONItemObject;
  versionData: TPasJSONItem;
begin
  Result := '4.0';
  if Assigned(jsobj) then
  begin
    x3dObj := TPasJSONItemObject(jsobj.Properties['X3D']);
    if Assigned(x3dObj) then
    begin
      versionData := x3dObj.Properties['@version'];
      if Assigned(versionData) then
        Result := StringReplace(TPasJSON.getString(versionData), '"', '', [rfReplaceAll]);
    end;
  end;
end;

function TX3DJSONLD.SerializeDOM(const x3dVersion: String; document: TDOMDocument): String;
var
  stringStream: TStringStream;
begin
  stringStream := TStringStream.Create('');
  try
    WriteXML(document, stringStream);
    Result := stringStream.DataString;
  finally
    stringStream.Free;
  end;
end;

end.
