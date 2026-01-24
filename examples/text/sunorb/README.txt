This example is a simple text based application that uses the messages from
Sun's ORB (Java).  A subset of the messages from the ORB implementation
message set is displayed for the current locale.

Since this example is based on an existing Java .properties file set, the
messages are externalized and are first compiled to the Ada package
Sun.ORB_Messages using zbmcompile.

Following the standard ZB convention, example applications are prefixed
with "x".

This example has no third party dependencies.

WARNING: Windows command windows do not support Unicode chararters.  This
example will display "garbage" if run on Windows for non-English locales.

Typical build and run sequence

-----------------------------------------------------------------------------
$ make
../../../bin/zbmcompile -i -v Sun.ORB_Messages -d mesg sunorb
This is ZBMCompile, Version 0.1.0 ALPHA (r1462) at 9:01 AM on 8/24/10
Copyright (c) 2009-2010, Michael Rohan.  All rights reserved
Loaded 759 messages for the facility "sunorb" (11 locales)
Loaded 1 facilities, 69 keys, 11 locales and 759 messages
Created the spec "Sun.ORB_Messages" in the file "sun-orb_messages.ads"
Created the body "Sun.ORB_Messages" in the file "sun-orb_messages.adb"
ZBMCompile completed at 9:01 AM on 8/24/10, elapsed time 0:00:02.721
gprbuild -p -aP../../../lib -P xsunorb.gpr
creating auto.cgpr
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 xsunorb.adb
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 sun.ads
gcc-4.4 -c -g -O3 -gnata -gnatVa -gnatQ -gnaty -gnatwae -gnat05 -gnatW8 sun-orb_messages.adb
gprbind xsunorb.bexch
gnatbind xsunorb.ali
gcc-4.4 -c b__xsunorb.adb
gcc-4.4 xsunorb.o -o xsunorb

-----------------------------------------------------------------------------
$ make run
gprbuild -p -aP../../../lib -P xsunorb.gpr
creating auto.cgpr
gprbuild: "xsunorb" up to date
../../../bin/xsunorb
caught exception while saving Properties to file |0|: Exception |1|
the file |0| not found
the file |0| is not readable
setting port to |0| and reading services from |1|
Usage: |0| <options> 

where <options> includes:
  -ORBInitialPort        Initial Port (required)
  -InitialServicesFile   File containing list of initial services (required)


Failed to start ORBD because ORBinitialPort is already in use

Failed to start ORBD because of an Internal Exception. 
Possible Causes: 
1. Specified ORBInitialPort or ORBActivationPort is already in use 
2. No Write Permission to write orb.db 
Usage: |0| <options> 

where <options> includes:
  -port                  Activation Port where the ORBD should be started, default 1049 (optional)
  -defaultdb             Directory for ORBD files, default "./orb.db" (optional)
  -serverid              Server Id for ORBD, default 1 (optional)
  -ORBInitialPort        Initial Port (required)
  -ORBInitialHost        Initial HostName (required)

Persistent NameServer Started Successfully
	applicationName     - |0|
	args      - |0|
Bad server definition: |0|


Welcome to the Java IDL Server Tool 
please enter commands at the prompt 

	classpath - |0|

	getserverid [ -applicationName <name> ] 

return the server id for an applicationName
	Server ID for applicationName |0| is |1|
	server is held down.
	help
	OR
	help <command name>

get help

	list

list all registered servers

	Server Id	Server Class Name		Server Application
	---------	-----------------		------------------


	listactive
list currently active servers
	listappnames

list applicationNames currently defined
Currently defined server applicationNames:

	locate [ -serverid <server id> | -applicationName <name> ] [ <-endpointType <endpointType> ] 

locate ports of specific type for a registered server


	Host Name |0| 

		Port		Port Type		ORB Id
		----		---------		------


	locateperorb [ -serverid <server id> | -applicationName <name> ] [ -orbid <ORB name> ]

locate ports for a specific orb of registered server


	Host Name |0| 

		Port		PortType		ORB Id
		----		--------		------

	name      - |0|
	invalid ORB.
	no such server found.
	Usage: orblist [ -serverid <server id> | -applicationName <name> ]

list of orb names and their mapping

	ORB Id		ORB Name
	------		--------


	quit

quit this tool


	register -server <server class name> 
	         -applicationName <alternate server name> 
	         -classpath <classpath to server> 
	         -args <args to server> 
	         -vmargs <args to server Java VM>

register an activatable server
	server registered (serverid = |0|).
	server registerd but held down (serverid = |0|).
	server already registered (serverid = |0|).
	server id - |0|
	server is not running.
	server is already up.


	Available Commands: 
	------------------- 


	shutdown [ -serverid <server id> | -applicationName <name> ]

shutdown a registered server
	server sucessfully shutdown.

	startup [ -serverid <server id> | -applicationName <name> ]

start a registered server
	server sucessfully started up.

	unregister [ -serverid <server id> | -applicationName <name> ] 

unregister a registered server
	server unregistered.
Usage: |0| <options> 

where <options> includes:
  -ORBInitialPort        Initial Port (required)
  -ORBInitialHost        Initial HostName (required)

	vmargs    - |0|
caught an exception while starting the bootstrap service on port |0|
Initial Naming Context:
|0|
TransientNameServer: setting port for initial object references to: |0|
Ready.
ORBInitialHost is not a valid option for NameService
ORBInitialPort 0 is not valid option for NameService
try using a different port with commandline arguments -ORBInitialPort <portno>

$ make run_zh_TW
gprbuild -p -aP../../../lib -P xsunorb.gpr
creating auto.cgpr
gprbuild: "xsunorb" up to date
ZB_LANG=zh_TW ../../../bin/xsunorb
將屬性儲存至檔案 |0| 時發生異常：異常 |1|
檔案 |0| 未找到
檔案 |0| 無法讀取
設定連接埠至 |0| 並從 |1| 讀取服務
用法： |0| <options> 

其中 <options> 包括：
  -ORBInitialPort        起始埠（必要）
  -InitialServicesFile   含有起啟服務清單的檔案（必要）


因為 ORBinitialPort 在使用中，所以無法啟動 ORBD。

因為內部發生異常，所以無法啟動 ORBD。 
可能的原因： 
1. 指定的 ORBInitialPort 或 ORBActivationPort 在使用中。 
2. 沒有寫入 orb.db 的許可權。 
用法：|0| <options> 

其中 <options> 包括：
  -port                  ORBD 應被啟動的啟動埠所在，預設為 1049 (可選)
  -defaultdb             ORBD 檔案的目錄，預設 "./orb.db" (可選)
  -serverid              ORBD 伺服器 Id，預設為 1 (可選)
  -ORBInitialPort        起始埠（必要）
  -ORBInitialHost        起始主機名稱（必要）

永久性 NameServer 開啟成功
	applicationName     - |0|
	args      - |0|
錯誤的伺服器定義： |0|


歡迎來到 Java IDL 伺服器工具 
請在提示處輸入指令 

	classpath - |0|

	getserverid [ -applicationName <name> ] 

針對一個 applicationName 傳回伺服器識別碼
	applicationName 的伺服器識別碼 |0| 為 |1|
	伺服器不在工作狀態。
	幫助
	OR
	help <command name>

取得幫助

	清單

列出所有註冊過的伺服器

	伺服器識別碼	伺服器類別名稱		伺服器應用程式
	---------	-----------------		------------------


	listactive
列出現在啟動的伺服器
	listappnames

列出現在被定義的 applicationNames
現在定義的伺服器 applicationNames：

	locate [ -serverid <server id> | -applicationName <name> ] [ <-endpointType <endpointType> ] 

針對一個已註冊過的伺服器尋找特定類型的連接埠


	主機名稱 |0| 

		Port		Port Type		ORB Id
		----		---------		------


	locateperorb [ -serverid <server id> | -applicationName <name> ] [ -orbid <ORB name> ]

針對已註冊過的伺服器的特定 orb 尋找連接埠。


	主機名稱 |0| 

		Port		PortType		ORB Id
		----		--------		------

	名稱      - |0|
	無效的 ORB.
	找不到這個伺服器。
	用法： orblist [ -serverid <server id> | -applicationName <name> ]

orb 名稱及其對映清單

	ORB Id		ORB 名稱
	------		--------


	離開

離開這個工具


	register -server<server class name> 
	         -applicationName <alternate server name> 
	         -classpath <classpath to server> 
	         -args <args to server> 
	         -vmargs <args to server Java VM>

註冊一個可啟動的伺服器
	已註冊過的伺服器 (serverid = |0|)。
	已註冊過的伺服器，但不在工作狀態 (serverid = |0|)。
	伺服器已註冊 (serverid = |0|)。
	伺服器 id - |0|
	伺服器未運作。
	伺服器已在工作狀態。


	現有的指令：
	------------------- 


	shutdown [ -serverid <server id> | -applicationName <name> ]

關閉一個註冊過的伺服器
	伺服器關閉成功。

	startup [ -serverid <server id> | -applicationName <name> ]

開啟一個註冊過的伺服器
	伺服器開啟成功。

	unregister [ -serverid <server id> | -applicationName <name> ] 

未註冊一個已註冊過的伺服器
	伺服器未註冊。
用法： |0| <options> 

where <options> 包括：
  -ORBInitialPort        起始埠（必要）
  -ORBInitialHost        起始主機名稱（必要）

	vmargs    - |0|
開啟 |0| 連接埠上的啟動程式服務時，發生異常
起始命名內文：
|0|
TransientNameServer: 針對起始物件參照，設定連接埠至：|0|
就緒。
ORBInitialHost 不是 NameService 的有效選項
ORBInitialPort 0 不是 NameService 的有效選項
試著利用含有指令行引數的不同連接埠 -ORBInitialPort <portno>
