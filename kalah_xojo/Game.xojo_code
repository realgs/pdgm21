#tag Class
Protected Class Game
	#tag Method, Flags = &h21
		Private Sub changeStatus()
		  If GameStatus=GameStatuses.MyTurn Then
		    GameStatus=GameStatuses.OpponentTurn
		  Else
		    GameStatus=GameStatuses.MyTurn
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function checkIfEnd() As boolean
		  For x As Integer = 1 To 6
		    If board(x) <> 0 Then
		      For y As Integer  = 8 To 13
		        If board(y) <> 0 Then Return False
		      Next
		      Return True
		    End If
		  Next
		  
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(id_game as Integer, my_id as Integer, seedsInGame as Integer)
		  Me.id_game=id_game
		  Me.my_id=my_id
		  Me.seedsInGame=seedsInGame
		  Me.last_movement=New Movement(-1, -1, DateTime.Now)
		  
		  
		  For i As Integer = 0 To 14
		    If i = 0 Or i = 7 Then
		      board.Add(0)
		    Else
		      board.add(seedsInGame)
		    End If
		  Next
		  
		  If my_id=0 Then 
		    GameStatus=GameStatuses.WaitingForOtherPlayer
		  Else
		    GameStatus=GameStatuses.OpponentTurn
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub move()
		  //Funkcja zwraca id gracza który powinien wykonać następny ruch
		  
		  Var boardIndex As Integer
		  Var seeds As Integer
		  Var id_player As Integer
		  
		  If last_movement.house=-1 Then Return
		  
		  boardIndex = last_movement.house
		  id_player = last_movement.id_player
		  seeds = board(boardIndex)
		  
		  If seeds = 0 Then
		    MessageBox("The house is empty, choose another one.")
		    Return 
		  ElseIf boardIndex = 0 Or boardIndex = 7 Then
		    MessageBox("Cannot move from base")
		    Return
		  Else
		    board(boardIndex) = 0
		    
		    While seeds <> 0
		      boardIndex = (boardIndex + 1) Mod 14
		      board(boardIndex) = board(boardIndex) +1
		      seeds = seeds - 1
		    Wend
		    
		    If id_player = 0 And boardIndex = 7 Then
		      If checkIfEnd Then 
		        whoWins
		        Return
		      end if
		      Return 
		    End If
		    
		    If id_player = 1 And boardIndex = 0 Then
		      If checkIfEnd Then 
		        whoWins 
		        Return
		      End If
		      Return 
		    End If
		    
		    If board(boardIndex) = 1 And boardIndex >= id_player * 7 + 1 And boardIndex <= id_player * 7 + 6 Then
		      board(7 * ((id_player + 1) Mod 2)) = board(7 * ((id_player + 1) Mod 2)) + board(boardIndex)
		      board(boardIndex)=0
		      board(7 * ((id_player + 1) Mod 2)) = board(7 * ((id_player + 1) Mod 2)) + board(14 - boardIndex)
		      board(14-boardIndex)=0
		      If checkIfEnd Then 
		        whoWins 
		        Return
		      End If
		    End If
		    
		    changeStatus
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub whoWins()
		  GameStatus=GameStatuses.TheEnd
		  
		  If board(0) < board(7) And my_id = 0 Or board(0) > board(7) And my_id = 1 Then
		    MessageBox("You win! :D")
		  ElseIf board(0) > board(7) And my_id = 0 Or board(0) < board(7) And my_id = 1 Then
		    MessageBox("Your opponent wins :(")
		  Else
		    MessageBox("Tie :)")
		  End If
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		board() As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		GameStatus As GameStatuses
	#tag EndProperty

	#tag Property, Flags = &h0
		id_game As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		last_movement As Movement
	#tag EndProperty

	#tag Property, Flags = &h0
		my_id As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		seedsInGame As Integer
	#tag EndProperty


	#tag Enum, Name = GameStatuses, Type = Integer, Flags = &h0
		WaitingForOtherPlayer
		  MyTurn
		  OpponentTurn
		TheEnd
	#tag EndEnum


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
		#tag ViewProperty
			Name="id_game"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="my_id"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="seedsInGame"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="GameStatus"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="GameStatuses"
			EditorType="Enum"
			#tag EnumValues
				"0 - WaitingForOtherPlayer"
				"1 - MyTurn"
				"2 - OpponentTurn"
				"3 - TheEnd"
			#tag EndEnumValues
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
