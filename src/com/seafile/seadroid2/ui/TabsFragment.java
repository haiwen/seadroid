package com.seafile.seadroid2.ui;

import android.content.Context;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.ViewPager;
import android.util.Log;
import android.view.ContextThemeWrapper;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.actionbarsherlock.app.SherlockFragment;
import com.seafile.seadroid2.R;
import com.viewpagerindicator.IconPagerAdapter;
import com.viewpagerindicator.TabPageIndicator;

public class TabsFragment extends SherlockFragment {
	 private static final String[] CONTENT = new String[] { "Libraries", "Activities" };
	    private static final int[] ICONS = new int[] {
	            R.drawable.perm_group_library,
	            R.drawable.perm_group_activity,
	    };

	    @Override
	    public void onCreate(Bundle savedInstanceState) {
	        super.onCreate(savedInstanceState);
	    }
	    
	    @Override
	    public View onCreateView(LayoutInflater inflater, ViewGroup container,
	        Bundle savedInstanceState) {
	    	
	    	final Context contextThemeWrapper = new ContextThemeWrapper(getActivity(), R.style.StyledIndicators);

	        // clone the inflater using the ContextThemeWrapper
	        LayoutInflater localInflater = inflater.cloneInContext(contextThemeWrapper);
	    	
	    	View root = localInflater.inflate(R.layout.tabs_main, container, false);
	    	FragmentPagerAdapter adapter = new SeafileTabsAdapter(getActivity().getSupportFragmentManager());

	        ViewPager pager = (ViewPager)root.findViewById(R.id.pager);
	        pager.setAdapter(adapter);

	        TabPageIndicator indicator = (TabPageIndicator)root.findViewById(R.id.indicator);
	        indicator.setViewPager(pager);
	        //pager.setCurrentItem(0, false);
	        return root;
	    }
	    
	    class SeafileTabsAdapter extends FragmentPagerAdapter implements IconPagerAdapter {
	        public SeafileTabsAdapter(FragmentManager fm) {
	            super(fm);
	        }

	        private ReposFragment reposFragment = null;
	        private ActivitiesFragment activitieFragment = null;
	        @Override
	        public Fragment getItem(int position) {
	        	switch(position) {
	        	case 0 :
	        		Log.e("tab", "repos create");
	        		if(reposFragment == null) {
	        			reposFragment = new ReposFragment();
	        		}
	        		return reposFragment;
	        	case 1 :
	        		Log.e("tab", "activities create");
	        		if(activitieFragment == null) {
	        			activitieFragment = new ActivitiesFragment();
	        		}
	        		return activitieFragment;
	        	default : 
	        		return new Fragment();
	        	}
	        }

	        @Override
	        public CharSequence getPageTitle(int position) {
	            return CONTENT[position % CONTENT.length].toUpperCase();
	        }

	        @Override 
	        public int getIconResId(int index) {
	          return ICONS[index];
	        }

	      @Override
	        public int getCount() {
	          return CONTENT.length;
	        }
	    }
}
