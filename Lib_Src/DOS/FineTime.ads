------------------------------------------------------------------------------
--  File:            FineTime.ads        (possibly extracted from PC_PAQS.ZIP)
--  Description:     Fine timer, up to ~10 KHz - the DOS value is 18.2 Hz
--  Date/version:    5.VI.1999
--  Author:          G. de Montmollin
------------------------------------------------------------------------------

package Fine_Timer is

   Counter: Natural:= 0;

   -- The procedures below should be called in the order they appear:

   procedure Install;
   procedure Set_timer_frequency( Hz: Float );

   -- The fine timing system is active here; counter and frequency can
   -- be changed at any time.

   procedure Restore_timer_frequency;
   procedure Uninstall;

end Fine_Timer;
