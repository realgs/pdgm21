#tag Module
Protected Module moduleDatabase
	#tag Method, Flags = &h0
		Function checkPlayers(game as Game) As Integer
		  Var command As String
		  Var answerFromDatabase As RowSet
		  
		  'command="create table if not exists game (" + _
		  '"id_game integer NOT NULL primary key auto_increment," +_ 
		  '"begin_seeds integer NOT NULL," + _
		  '"current_players integer NOT NULL," + _
		  '"date_time timestamp NOT NULL" + _
		  '")"
		  
		  command="select current_players from game where id_game = "+game.id_game.ToString
		  
		  //połączenie z bazą
		  If Not connectDatabase Then Return -1
		  
		  Try
		    answerFromDatabase = sqldatabase.SelectSQL(command)
		    Return answerFromDatabase.ColumnAt(0).IntegerValue
		  Catch
		    MessageBox("Cannot create the game")
		    Return -1
		  End
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function connectDatabase() As Boolean
		  sqldatabase.Host=moduleDatabase.database_host
		  sqldatabase.Password=moduleDatabase.database_password
		  sqldatabase.UserName=moduleDatabase.database_user
		  sqldatabase.DatabaseName=moduleDatabase.database_name
		  
		  Try
		    sqldatabase.Connect
		    //MessageBox("Connected")
		  Catch
		    MessageBox("Cannot connect to database")
		    Return False
		  End
		  
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub createDatabase()
		  Var command As String
		  
		  //stworzenie bazy
		  sqldatabase = New MySQLCommunityServer
		  
		  //połączenie z bazą
		  If Not connectDatabase Then Return
		  
		  
		  //stworzenie tabeli w bazie
		  
		  //tabela gra
		  command="create table if not exists game (" + _
		  "id_game integer NOT NULL primary key auto_increment," +_ 
		  "begin_seeds integer NOT NULL," + _
		  "current_players integer NOT NULL," + _
		  "date_time timestamp NOT NULL" + _
		  ")"
		  
		  Try
		    sqldatabase.ExecuteSQL(command)
		  Catch
		    MessageBox("Couldn't create table 'GAME'")
		    Return
		  End 
		  
		  
		  //tabela ruchy graczy
		  command="create table if not exists movements (" + _
		  "id_movement integer NOT NULL primary key auto_increment," + _
		  "id_game integer NOT NULL," +_ 
		  "id_player integer NOT NULL," + _
		  "house integer NOT NULL," + _
		  "date_time timestamp NOT NULL" + _
		  ")"
		  
		  Try
		    sqldatabase.ExecuteSQL(command)
		  Catch
		    MessageBox("Couldn't create table 'MOVEMENTS'")
		    Return
		  End 
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub createGame(begin_seeds as Integer)
		  Var command As String
		  
		  'command="create table if not exists game (" + _
		  '"id_game integer NOT NULL primary key auto_increment," +_ 
		  '"begin_seeds integer NOT NULL," + _
		  '"current_players integer NOT NULL," + _
		  '"date_time timestamp NOT NULL" + _
		  '")"
		  
		  command="insert into game (begin_seeds, current_players) values ("+begin_seeds.ToString+", 0"+")"
		  
		  //połączenie z bazą
		  If Not connectDatabase Then Return
		  
		  Try
		    sqldatabase.ExecuteSQL(command)
		  Catch
		    MessageBox("Cannot create the game")
		    Return
		  End
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function enterGame(id_game as Integer) As Game
		  Dim command As String
		  Dim answerFromDatabase As RowSet
		  Dim row As DatabaseRow
		  Dim newGame As Game
		  Dim begin_seeds As Integer 
		  Dim current_players As Integer
		  Dim movements As Integer
		  
		  'command="create table if not exists game (" + _
		  '"id_game integer NOT NULL primary key auto_increment," +_ 
		  '"begin_seeds integer NOT NULL," + _
		  '"current_players integer NOT NULL," + _
		  '"date_time timestamp NOT NULL" + _
		  '")"
		  
		  command="select begin_seeds, current_players, (select count(id_movement) from movements where id_game ="+_
		  id_game.ToString+") from game where id_game ="+id_game.ToString
		  
		  //połączenie z bazą
		  If Not connectDatabase Then Return Nil
		  
		  //pobranie danych gry
		  Try
		    answerFromDatabase = sqldatabase.SelectSQL(command)
		    If answerFromDatabase.RowCount<> 0 Then
		      begin_seeds = answerFromDatabase.ColumnAt(0).IntegerValue
		      current_players =  answerFromDatabase.ColumnAt(1).IntegerValue
		      movements = answerFromDatabase.ColumnAt(2).IntegerValue
		      If movements > 0 Then
		        MessageBox("The game has been started")
		        Return Nil
		      End If
		      If current_players < 2 Then
		        newGame=New Game(id_game, current_players, begin_seeds)
		      Else
		        MessageBox("The game is full.")
		        Return Nil
		      End If
		    Else
		      MessageBox("There is no game with the ID.")
		      Return Nil
		    End If
		  Catch
		    MessageBox("Cannot enter the game")
		    Return Nil
		  End Try
		  
		  //dodanie gracza do gry
		  command = "update game set current_players = current_players + 1 where id_game like "+id_game.ToString
		  
		  Try
		    sqldatabase.ExecuteSQL(command)
		  Catch
		    MessageBox("Cannot add player to the game")
		  End Try
		  
		  Return newGame
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getIdGame() As Integer
		  Var command As String
		  Var answerFromDatabase As RowSet
		  
		  'command="create table if not exists game (" + _
		  '"id_game integer NOT NULL primary key auto_increment," +_ 
		  '"begin_seeds integer NOT NULL," + _
		  '"current_players integer NOT NULL," + _
		  '"date_time timestamp NOT NULL" + _
		  '")"
		  
		  command="select id_game from game order by date_time desc limit 1"
		  
		  //połączenie z bazą
		  If Not connectDatabase Then Return -1
		  
		  Try
		    answerFromDatabase = sqldatabase.SelectSQL(command)
		    Return answerFromDatabase.ColumnAt(0).IntegerValue
		  Catch
		    MessageBox("Cannot create the game")
		    Return -1
		  End
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function getSendingTime(id_game as Integer) As DateTime
		  Dim command As String
		  Dim answerFromDatabase As RowSet
		  
		  command="select date_time from movements where id_game ="+id_game.ToString+_
		  " order by date_time desc limit 1"
		  
		  //połączenie z bazą
		  If Not connectDatabase Then Return Nil
		  
		  Try
		    answerFromDatabase = sqldatabase.SelectSQL(command)
		    Return answerFromDatabase.ColumnAt(0).DateTimeValue
		  Catch
		    MessageBox("Cannot get the sending time")
		    Return Nil
		  End
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub leaveGame(game as Game)
		  Dim command As String
		  
		  command="UPDATE game SET current_players = current_players-1 WHERE id_game = "+_
		  game.id_game.ToString 
		  
		  Try
		    sqldatabase.ExecuteSQL(command)
		  Catch
		    MessageBox("Cannot leave the game")
		  End Try
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function sendMovement(id_game as Integer, id_player as Integer, house as Integer) As Movement
		  Dim command As String
		  Dim answerFromDatabase As RowSet
		  Dim row As DatabaseRow
		  
		  '//tabela ruchy graczy
		  'command="create table if not exists movements (" + _
		  '"id_movement integer NOT NULL primary key auto_increment," + _
		  '"id_game integer NOT NULL," +_ 
		  '"id_player integer NOT NULL," + _
		  '"house integer NOT NULL," + _
		  '"date_time timestamp NOT NULL" + _
		  '")"
		  
		  command="insert into movements (id_game, id_player, house) values ("+_
		  id_game.ToString+", "+_
		  id_player.ToString+", "+_
		  house.ToString+_
		  ")"
		  
		  
		  Try
		    sqldatabase.ExecuteSQL(command)
		    Return New Movement(id_player, house, getSendingTime(id_game))
		  Catch
		    MessageBox("Could not sent movement")
		    Return Nil
		  End Try
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function tryGetMovement(game as Game) As Movement
		  Dim command As String
		  Dim answerFromDatabase As RowSet
		  Dim current_time As DateTime = DateTime.Now
		  Dim last_movement_time As DateTime
		  Dim id_player As Integer
		  Dim house As Integer
		  Dim newMovement As Movement
		  
		  '//tabela ruchy graczy
		  'command="create table if not exists movements (" + _
		  '"id_movement integer NOT NULL primary key auto_increment," + _
		  '"id_game integer NOT NULL," +_ 
		  '"id_player integer NOT NULL," + _
		  '"house integer NOT NULL," + _
		  '"date_time timestamp NOT NULL" + _
		  '")"
		  
		  command="select * from movements where id_game = "+game.id_game.ToString+" order by date_time desc limit 1"
		  
		  //połączenie z bazą
		  If Not connectDatabase Then Return Nil
		  
		  Try
		    answerFromDatabase = sqldatabase.SelectSQL(command)
		    
		    If answerFromDatabase.RowCount <> 0 Then
		      id_player = answerFromDatabase.ColumnAt(2).IntegerValue
		      house = answerFromDatabase.ColumnAt(3).IntegerValue
		      last_movement_time = answerFromDatabase.ColumnAt(4).DateTimeValue
		      
		      
		      If last_movement_time.SQLDateTime <> game.last_movement.last_movement_time.SQLDateTime Then
		        newMovement=New Movement(id_player, house, last_movement_time)
		        Return newMovement
		      Else
		        Return Nil
		      End If
		      
		    End If
		    
		    Return Nil
		  Catch
		    MessageBox("Could not get the last movement")
		    Return Nil
		  End
		End Function
	#tag EndMethod


	#tag Property, Flags = &h21
		Private sqldatabase As MySQLCommunityServer
	#tag EndProperty


	#tag Constant, Name = database_host, Type = String, Dynamic = False, Default = \"mysql-paulina.halpress.eu", Scope = Private
	#tag EndConstant

	#tag Constant, Name = database_name, Type = String, Dynamic = False, Default = \"db100061665", Scope = Private
	#tag EndConstant

	#tag Constant, Name = database_password, Type = String, Dynamic = False, Default = \"niepodamhasla", Scope = Private
	#tag EndConstant

	#tag Constant, Name = database_user, Type = String, Dynamic = False, Default = \"db100061665", Scope = Private
	#tag EndConstant


	#tag ViewBehavior
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
