package com.seafile.seadroid2.ui.activity;

import android.os.Bundle;
import android.util.Log;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.MenuItem;
import com.seafile.seadroid2.R;

/**
 *
 */
public class SwipeBackActivity extends SherlockFragmentActivity {

    @Override
    public void onCreate(Bundle saved) {
        super.onCreate(saved);

        Log.d("SwipeBack", "onCreate");

        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setHomeButtonEnabled(true);

    }

    @Override
    public void onDestroy() {
        super.onDestroy();

        Log.d("SwipeBack", "onDestroy");

    }


    @Override
    public void onBackPressed() {
        super.onBackPressed();
        /*overridePendingTransition(R.anim.swipeback_stack_to_front,
                R.anim.swipeback_stack_right_out);*/
    }


    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            onBackPressed();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

}
