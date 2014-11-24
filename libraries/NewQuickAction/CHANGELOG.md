Change Log
===============================================================================

Version 1.0.0 *(2011-10-20)*
----------------------------

* Fix transparent background bug. Thanx to [Roman Wozniak](roman.wozniak@gmail.com).
2. New improvements added by [Kevin Pack](kevinpeck@gmail.com):
  3. Action Item – new constructor with action id, title, icon
  4. Callback enhanced to return QuickAction object as source and action id (allows you to add items in any order as you base what was clicked on by the ID, not the pos)
  5. Action item supports sticky mode, if that is enabled the menu does not dismiss post button press. I needed this for my application.
  6. QuickAction has getActionItem(pos) call so you can get action item back. QuickAction has ArrayList of added items to support this
  7. Updated NewQuickAction3DActivity to show the toast message based on label of action item clicked as you now have enough info in callback to do that generically
3. New listener to handle on dismiss event.
4. Fix force close that occured when tapping randomly on a view to show QuickAction dialog ([issue](https://github.com/lorensiuswlt/NewQuickAction3D/issues/2)). Thanx to [Zammbi](zammbi@gmail.com) for bug fixing..
5. Add __Apache License, Version 2.0__ to this source code (see LICENSE.txt)