PROGRAM main
    IMPLICIT NONE;
    INTEGER :: i;
    CHARACTER(len=32) :: arg;
    
    DO i = 1, COMMAND_ARGUMENT_COUNT();
        CALL GET_COMMAND_ARGUMENT(i, arg)
        PRINT *, "argument: ", arg;
    ENDDO;
ENDPROGRAM
