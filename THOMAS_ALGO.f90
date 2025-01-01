!****************************************************************************                                                                         
! *   FILE         = THOMAS_ALGO.F90                                        *
! *   AUTHOR       = CHANDAN KUMAR (CHANDANKR@IITKGP.AC.IN)                 *
! *   INSTITUTE    = INDIAN INSTITUTE OF TECHNOLOGY (IIT), KHARAGPUR        *
!****************************************************************************
! *  THIS PROGRAM IS DISTRIBUTED IN A HOPE THAT IT WILL BE USEFUL.          *                                                                        
!**************************************************************************** 
      PROGRAM THOMAMS_ALGO
      IMPLICIT NONE
      INTEGER, PARAMETER :: N=8
      DOUBLE PRECISION :: A(N,N), B(N), X(N), D(N), C(N), E(N), F(N)
      INTEGER :: I,J
      
       

      A(1,:) = (/1,10,0,0,0,0,0,0/)
      A(2,:) = (/5,2,8,0,0,0,0,0/)
      A(3,:) = (/0,13,4,6,0,0,0,0/)
      A(4,:) = (/0,0,1,1,7,0,0,0/)
      A(5,:) = (/0,0,0,4,3,5,0,0/)
      A(6,:) = (/0,0,0,0,1,6,3,0/)
      A(7,:) = (/0,0,0,0,0,4,9,1/)
      A(8,:) =(/0,0,0,0,0,0,2,5/)



      B(:) = (/3,2,5,7,8,9,4,1/)

      ! STORE LOWER DIAGONAL IN C, MAIN DIAGONAL IN D, UPPER DIAGONAL IN E AND VECTOR B IN F 
        J =0
      DO I =2, N
           J=J+1
          C(I) = A(I,J)
      END DO

      DO I = 1, N
        D(I) = A(I,I)
      END DO

       J=1
      DO I =1, N-1
           J=J+1
          E(I) = A(I,J)
      END DO

      DO I =1,N
         F(I) = B(I)
      END DO

      DO I =2,N
         D(I) = D(I) - C(I)*E(I-1)/D(I-1)
      END DO

      DO I = 2,N
        F(I) = F(I) -F(I-1)*C(I)/D(I-1)
      END DO

        X(N) = F(N)/D(N)

      DO I = N-1, 1,-1
         X(I) = ( F(I) - E(I) * X(I+1))/D(I)
      END DO

      PRINT*, 'SOLUTION VECTOR:'
      DO I =1, N
         PRINT *, X(I)
      END DO

   END PROGRAM


