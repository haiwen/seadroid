package com.seafile.seadroid2.ui;

import com.actionbarsherlock.view.ActionMode;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.seafile.seadroid2.R;

/**
 * Represents a contextual mode of the user interface.
 * Action modes can be used to provide alternative interaction modes and replace parts of the normal UI until finished.
 * A Callback configures and handles events raised by a user's interaction with an action mode.
 */
public class ActionModeCallback implements ActionMode.Callback {
    private boolean allItemsSelected;
    private ActionModeOperationListener mListener;

    public ActionModeCallback(ActionModeOperationListener listener) {
        mListener = listener;
    }

    public interface ActionModeOperationListener {
        void selectItems();
        void deselectItems();
        void onActionModeDestroy();
    }

    @Override
    public boolean onCreateActionMode(ActionMode mode, Menu menu) {
        // Inflate the menu for the contextual action bar (CAB)
        MenuInflater inflater = mode.getMenuInflater();
        inflater.inflate(R.menu.transfer_list_multi_choice_menu, menu);
        return true;
    }

    @Override
    public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
        // Here you can perform updates to the contextual action bar (CAB) due to
        // an invalidate() request
        return false;
    }

    @Override
    public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
        // Respond to clicks on the actions in the contextual action bar (CAB)
        switch (item.getItemId()) {
            case R.id.transfer_mode_select_all:
                if (!allItemsSelected)
                    mListener.selectItems();
                else
                    mListener.deselectItems();

                allItemsSelected = !allItemsSelected;

                return true;
            default:
                return false;
        }
    }

    @Override
    public void onDestroyActionMode(ActionMode mode) {
        mListener.onActionModeDestroy();
        mListener = null;
    }

}

