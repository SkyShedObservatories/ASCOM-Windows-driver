'tabs=4
' --------------------------------------------------------------------------------
' TODO fill in this information for your driver, then remove this line!
'
' ASCOM Dome driver for SkyShedPODS
'
' Description:	Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam 
'				nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam 
'				erat, sed diam voluptua. At vero eos et accusam et justo duo 
'				dolores et ea rebum. Stet clita kasd gubergren, no sea takimata 
'				sanctus est Lorem ipsum dolor sit amet.
'
' Implements:	ASCOM Dome interface version: 1.0
' Author:		(XXX) Your N. Here <your@email.here>
'
' Edit Log:
'
' Date			Who	Vers	Description
' -----------	---	-----	-------------------------------------------------------
' dd-mmm-yyyy	XXX	1.0.0	Initial edit, from Dome template
' ---------------------------------------------------------------------------------
'
'
' Your driver's ID is ASCOM.SkyShedPODS.Dome
'
' The Guid attribute sets the CLSID for ASCOM.DeviceName.Dome
' The ClassInterface/None attribute prevents an empty interface called
' _Dome from being created and used as the [default] interface
'

' This definition is used to select code that's only applicable for one device type
#Const Device = "Dome"

Imports System
Imports System.Collections
Imports System.Collections.Generic
Imports System.Globalization
Imports System.Runtime.InteropServices
Imports System.Text
Imports ASCOM
Imports ASCOM.Astrometry
Imports ASCOM.Astrometry.AstroUtils
Imports ASCOM.DeviceInterface
Imports ASCOM.Utilities

<Guid("c34cd60d-e2a9-44c2-840d-a91ad60bab62")>
<ClassInterface(ClassInterfaceType.None)>
Public Class Dome

    ' The Guid attribute sets the CLSID for ASCOM.SkyShedPODS.Dome
    ' The ClassInterface/None attribute prevents an empty interface called
    ' _SkyShedPODS from being created and used as the [default] interface

    ' TODO Replace the not implemented exceptions with code to implement the function or
    ' throw the appropriate ASCOM exception.
    '
    Implements IDomeV2


    '
    ' Driver ID and descriptive string that shows in the Chooser
    '
    Friend Shared driverID As String = "ASCOM.SkyShed-POD-S.Dome"
    Private Shared driverDescription As String = "SkyShed-POD-S Dome"
    Private objSerial As ASCOM.Utilities.Serial


    Friend Shared comPortProfileName As String = "COM Port" 'Constants used for Profile persistence
    Friend Shared traceStateProfileName As String = "Trace Level"
    Friend Shared comPortDefault As String = "COM5"
    Friend Shared traceStateDefault As String = "False"

    Friend Shared comPort As String ' Variables to hold the current device configuration
    Friend Shared traceState As Boolean

    Private connectedState As Boolean ' Private variable to hold the connected state
    Private utilities As Util ' Private variable to hold an ASCOM Utilities object
    Private astroUtilities As AstroUtils ' Private variable to hold an AstroUtils object to provide the Range method
    Private TL As TraceLogger ' Private variable to hold the trace logger object (creates a diagnostic log file with information that you specify)

    '
    ' Constructor - Must be public for COM registration!
    '
    Public Sub New()

        ReadProfile() ' Read device configuration from the ASCOM Profile store
        TL = New TraceLogger("", "SkyShed-POD-S")
        TL.Enabled = traceState
        TL.LogMessage("Dome", "Starting initialisation")

        connectedState = False ' Initialise connected to false
        utilities = New Util() ' Initialise util object
        astroUtilities = New AstroUtils 'Initialise new astro utilities object

        'TODO: Implement your additional construction here

        TL.LogMessage("Dome", "Completed initialisation")
    End Sub

    '
    ' PUBLIC COM INTERFACE IDomeV2 IMPLEMENTATION
    '

#Region "Common properties and methods"
    ''' <summary>
    ''' Displays the Setup Dialog form.
    ''' If the user clicks the OK button to dismiss the form, then
    ''' the new settings are saved, otherwise the old values are reloaded.
    ''' THIS IS THE ONLY PLACE WHERE SHOWING USER INTERFACE IS ALLOWED!
    ''' </summary>
    Public Sub SetupDialog() Implements IDomeV2.SetupDialog
        ' consider only showing the setup dialog if not connected
        ' or call a different dialog if connected
        If IsConnected Then
            System.Windows.Forms.MessageBox.Show("Already connected, just press OK")
        End If

        Using F As SetupDialogForm = New SetupDialogForm()
            Dim result As System.Windows.Forms.DialogResult = F.ShowDialog()
            If result = DialogResult.OK Then
                WriteProfile() ' Persist device configuration values to the ASCOM Profile store
            End If
        End Using
    End Sub

    Public ReadOnly Property SupportedActions() As ArrayList Implements IDomeV2.SupportedActions
        Get
            Dim x As Int32
            Dim features As New ArrayList
            Dim s As String
            x = 0
            Do
                s = "$GetSupportedActions=" + x.ToString + "#"
                objSerial.Transmit(s)
                s = objSerial.ReceiveTerminated("#")
                s = s.Replace("#", "")
                If s.Length > 0 Then
                    features.Add(s)
                End If
                x = x + 1
            Loop While s.Length > 1
            TL.LogMessage("SupportedActions Get", "Returning empty arraylist")
            Return features
        End Get
    End Property

    Public Function Action(ByVal ActionName As String, ByVal ActionParameters As String) As String Implements IDomeV2.Action
        Throw New ActionNotImplementedException("Action " & ActionName & " is not supported by this driver")
    End Function

    Public Sub CommandBlind(ByVal Command As String, Optional ByVal Raw As Boolean = False) Implements IDomeV2.CommandBlind
        CheckConnected("CommandBlind")
        objSerial.Transmit("$PutCommandBlind#")
    End Sub

    Public Function CommandBool(ByVal Command As String, Optional ByVal Raw As Boolean = False) As Boolean _
        Implements IDomeV2.CommandBool
        CheckConnected("CommandBool")
        objSerial.Transmit("$PutCommandBool#")
        Dim s As String
        s = objSerial.ReceiveTerminated("#")
        Dim retBool As Boolean
        retBool = s.Equals("True#")

        ' Dim retString as String = CommandString(command, raw) ' Send the command And wait for the response
        ' Dim retBool as Boolean = XXXXXXXXXXXXX ' Parse the returned string And create a boolean True / False value
        ' Return retBool ' Return the boolean value to the client

        Return retBool
    End Function

    Public Function CommandString(ByVal Command As String, Optional ByVal Raw As Boolean = False) As String _
        Implements IDomeV2.CommandString
        CheckConnected("CommandString")
        objSerial.Transmit("$PutCommandBool#")
        Dim s As String
        s = objSerial.ReceiveTerminated("#")
        s = s.Replace("#", "")
        Return s
    End Function

    Public Property Connected() As Boolean Implements IDomeV2.Connected
        Get
            TL.LogMessage("Get Connected", IsConnected.ToString())
            Return IsConnected
        End Get
        Set(value As Boolean)
            TL.LogMessage("Set Connected", value.ToString())
            If value = IsConnected Then
                Return
            End If

            If value Then
                connectedState = True
                Dim portNumber As String
                portNumber = comPort.Substring(3)
                objSerial = New ASCOM.Utilities.Serial
                objSerial.Port = Convert.ToInt32(portNumber)
                TL.LogMessage("Connected Set", "Connecting to port " + objSerial.Port.ToString)
                objSerial.Speed = 9600
                objSerial.Connected = True
            Else
                connectedState = False
                TL.LogMessage("Connected Set", "Disconnecting from port " + comPort)
                objSerial.Connected = False
            End If
        End Set
    End Property

    Public ReadOnly Property Description As String Implements IDomeV2.Description
        Get
            objSerial.Transmit("$GetDescription#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            s = s.Replace("#", "")
            Return s
        End Get
    End Property

    Public ReadOnly Property DriverInfo As String Implements IDomeV2.DriverInfo
        Get
            Dim m_version As Version = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version
            Dim s_driverInfo As String = "Dome Control: Driver version: " + m_version.Major.ToString() + "." + m_version.Minor.ToString()
            TL.LogMessage("DriverInfo Get", s_driverInfo)
            Return s_driverInfo
        End Get
    End Property

    Public ReadOnly Property DriverVersion() As String Implements IDomeV2.DriverVersion
        Get
            ' Get our own assembly and report its version number
            TL.LogMessage("DriverVersion Get", Reflection.Assembly.GetExecutingAssembly.GetName.Version.ToString(2))
            Return Reflection.Assembly.GetExecutingAssembly.GetName.Version.ToString(2)
        End Get
    End Property

    Public ReadOnly Property InterfaceVersion() As Short Implements IDomeV2.InterfaceVersion
        Get
            TL.LogMessage("InterfaceVersion Get", "2")
            Return 2
        End Get
    End Property

    Public ReadOnly Property Name As String Implements IDomeV2.Name
        Get
            objSerial.Transmit("$GetName#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            s = s.Replace("#", "")
            Return s
        End Get
    End Property

    Public Sub Dispose() Implements IDomeV2.Dispose
        ' Clean up the trace logger and util objects
        TL.Enabled = False
        TL.Dispose()
        TL = Nothing
        utilities.Dispose()
        utilities = Nothing
        astroUtilities.Dispose()
        astroUtilities = Nothing
    End Sub

#End Region

#Region "IDome Implementation"

    Private domeShutterState As Boolean = False ' Variable to hold the open/closed status of the shutter, true = Open

    Public Sub AbortSlew() Implements IDomeV2.AbortSlew
        objSerial.Transmit("$PutAbortSlew#")
        TL.LogMessage("AbortSlew", "Completed")
    End Sub

    Public ReadOnly Property Altitude() As Double Implements IDomeV2.Altitude
        Get
            TL.LogMessage("Get Altitude", "Not implemented")
            Throw New ASCOM.PropertyNotImplementedException("Altitude", False)
        End Get
    End Property

    Public ReadOnly Property AtHome() As Boolean Implements IDomeV2.AtHome
        Get
            objSerial.Transmit("$GetAtHome#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            TL.LogMessage("Get AtHome", s)
            Dim retBool As Boolean
            retBool = s.Equals("True#")
            Return retBool
        End Get
    End Property

    Public ReadOnly Property AtPark() As Boolean Implements IDomeV2.AtPark
        Get
            objSerial.Transmit("$GetAtPark#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            TL.LogMessage("Get AtPark", s)
            Dim retBool As Boolean
            retBool = s.Equals("True#")
            Return retBool
        End Get
    End Property

    Public ReadOnly Property Azimuth() As Double Implements IDomeV2.Azimuth
        Get
            objSerial.Transmit("$GetAzimuth#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            s = s.Replace("#", "")
            TL.LogMessage("Get Azimuth", s)
            Return CDbl(s)
        End Get
    End Property

    Public ReadOnly Property CanFindHome() As Boolean Implements IDomeV2.CanFindHome
        Get
            objSerial.Transmit("$GetCanFindHome#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            TL.LogMessage("Get CanFindHome", s)
            Dim retBool As Boolean
            retBool = s.Equals("True#")
        End Get
    End Property

    Public ReadOnly Property CanPark() As Boolean Implements IDomeV2.CanPark
        Get
            objSerial.Transmit("$GetCanPark#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            TL.LogMessage("Get CanPark", s)
            Dim retBool As Boolean
            retBool = s.Equals("True#")
        End Get
    End Property

    Public ReadOnly Property CanSetAltitude() As Boolean Implements IDomeV2.CanSetAltitude
        Get
            objSerial.Transmit("$GetCanSetAltitude#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            TL.LogMessage("Get CanSetAltitude", s)
            Dim retBool As Boolean
            retBool = s.Equals("True#")
            Return False
        End Get
    End Property

    Public ReadOnly Property CanSetAzimuth() As Boolean Implements IDomeV2.CanSetAzimuth
        Get
            objSerial.Transmit("$GetCanSetAzimuth#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            TL.LogMessage("Get CanSetAzimuth", s)
            Dim retBool As Boolean
            retBool = s.Equals("True#")
        End Get
    End Property

    Public ReadOnly Property CanSetPark() As Boolean Implements IDomeV2.CanSetPark
        Get
            objSerial.Transmit("$GetCanSetPark#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            TL.LogMessage("Get CanSetPark", s)
            Dim retBool As Boolean
            retBool = s.Equals("True#")
        End Get
    End Property

    Public ReadOnly Property CanSetShutter() As Boolean Implements IDomeV2.CanSetShutter
        Get
            objSerial.Transmit("$GetCanSetShutter#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            TL.LogMessage("Get CanSetShutter", s)
            Dim retBool As Boolean
            retBool = s.Equals("True#")
        End Get
    End Property

    Public ReadOnly Property CanSlave() As Boolean Implements IDomeV2.CanSlave
        Get
            objSerial.Transmit("$GetCanSlave#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            TL.LogMessage("Get CanSlave", s)
            Dim retBool As Boolean
            retBool = s.Equals("True#")
        End Get
    End Property

    Public ReadOnly Property CanSyncAzimuth() As Boolean Implements IDomeV2.CanSyncAzimuth
        Get
            objSerial.Transmit("$GetCanSyncAzimuth#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            TL.LogMessage("Get CanSyncAzimuth", s)
            Dim retBool As Boolean
            retBool = s.Equals("True#")
        End Get
    End Property

    Public Sub CloseShutter() Implements IDomeV2.CloseShutter
        objSerial.Transmit("$PutCloseShutter#")
        TL.LogMessage("Put CloseShutter", "")
    End Sub

    Public Sub FindHome() Implements IDomeV2.FindHome
        objSerial.Transmit("$PutFindHome#")
        TL.LogMessage("Put FindHome", "")
    End Sub

    Public Sub OpenShutter() Implements IDomeV2.OpenShutter
        objSerial.Transmit("$PutOpenShutter#")
        TL.LogMessage("OpenShutter", "Shutter has been opened")
    End Sub

    Public Sub Park() Implements IDomeV2.Park
        objSerial.Transmit("$PutPark#")
        TL.LogMessage("Park", "Dome has been parked")
    End Sub

    Public Sub SetPark() Implements IDomeV2.SetPark
        TL.LogMessage("SetPark", "Not implemented")
        Throw New ASCOM.MethodNotImplementedException("SetPark")
    End Sub

    Public ReadOnly Property ShutterStatus() As ShutterState Implements IDomeV2.ShutterStatus
        Get
            objSerial.Transmit("$GetShutterStatus#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            TL.LogMessage("Get ShutterStatus", s)
            If (s.Equals("Open#")) Then
                Return ShutterState.shutterOpen
            Else
                Return ShutterState.shutterClosed
            End If
        End Get
    End Property

    Public Property Slaved() As Boolean Implements IDomeV2.Slaved
        Get
            TL.LogMessage("Get Slaved", False.ToString())
            Return False
        End Get
        Set(value As Boolean)
            TL.LogMessage("Put Slaved", "not implemented")
            Throw New ASCOM.PropertyNotImplementedException("Slaved", True)
        End Set
    End Property

    Public Sub SlewToAltitude(Altitude As Double) Implements IDomeV2.SlewToAltitude
        TL.LogMessage("SlewToAltitude", "Not implemented")
        Throw New ASCOM.MethodNotImplementedException("SlewToAltitude")
    End Sub

    Public Sub SlewToAzimuth(Azimuth As Double) Implements IDomeV2.SlewToAzimuth
        Dim s As String = "$PutSlewToAzimuth=" + Azimuth.ToString + "#"
        objSerial.Transmit(s)
        TL.LogMessage("SlewToAzimuth", "")
    End Sub

    Public ReadOnly Property Slewing() As Boolean Implements IDomeV2.Slewing
        Get
            objSerial.Transmit("$GetSlewing#")
            Dim s As String
            s = objSerial.ReceiveTerminated("#")
            TL.LogMessage("Get Slewing", s)
            Dim retBool As Boolean
            retBool = s.Equals("True#")
        End Get
    End Property

    Public Sub SyncToAzimuth(Azimuth As Double) Implements IDomeV2.SyncToAzimuth
        TL.LogMessage("SyncToAzimuth", "Not implemented")
        Throw New ASCOM.MethodNotImplementedException("SyncToAzimuth")
    End Sub

#End Region

#Region "Private properties and methods"
    ' here are some useful properties and methods that can be used as required
    ' to help with

#Region "ASCOM Registration"

    Private Shared Sub RegUnregASCOM(ByVal bRegister As Boolean)

        Using P As New Profile() With {.DeviceType = "Dome"}
            If bRegister Then
                P.Register(driverID, driverDescription)
            Else
                P.Unregister(driverID)
            End If
        End Using

    End Sub

    <ComRegisterFunction()>
    Public Shared Sub RegisterASCOM(ByVal T As Type)

        RegUnregASCOM(True)

    End Sub

    <ComUnregisterFunction()>
    Public Shared Sub UnregisterASCOM(ByVal T As Type)

        RegUnregASCOM(False)

    End Sub

#End Region

    ''' <summary>
    ''' Returns true if there is a valid connection to the driver hardware
    ''' </summary>
    Private ReadOnly Property IsConnected As Boolean
        Get
            ' TODO check that the driver hardware connection exists and is connected to the hardware
            If Not objSerial Is Nothing Then
                If objSerial.Connected Then
                    Return True
                Else
                    Return False
                End If
            Else
                Return False
            End If
        End Get
    End Property

    ''' <summary>
    ''' Use this function to throw an exception if we aren't connected to the hardware
    ''' </summary>
    ''' <param name="message"></param>
    Private Sub CheckConnected(ByVal message As String)
        If Not IsConnected Then
            Throw New NotConnectedException(message)
        End If
    End Sub

    ''' <summary>
    ''' Read the device configuration from the ASCOM Profile store
    ''' </summary>
    Friend Sub ReadProfile()
        Using driverProfile As New Profile()
            driverProfile.DeviceType = "Dome"
            traceState = Convert.ToBoolean(driverProfile.GetValue(driverID, traceStateProfileName, String.Empty, traceStateDefault))
            comPort = driverProfile.GetValue(driverID, comPortProfileName, String.Empty, comPortDefault)
        End Using
    End Sub

    ''' <summary>
    ''' Write the device configuration to the  ASCOM  Profile store
    ''' </summary>
    Friend Sub WriteProfile()
        Using driverProfile As New Profile()
            driverProfile.DeviceType = "Dome"
            driverProfile.WriteValue(driverID, traceStateProfileName, traceState.ToString())
            driverProfile.WriteValue(driverID, comPortProfileName, comPort.ToString())
        End Using

    End Sub

#End Region

End Class
