package com.seafile.seadroid2.ui;

import android.content.Context;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.ViewPager;
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
	    	
	        return root;
	    }
	    
	    class SeafileTabsAdapter extends FragmentPagerAdapter implements IconPagerAdapter {
	        public SeafileTabsAdapter(FragmentManager fm) {
	            super(fm);
	        }

	        @Override
	        public Fragment getItem(int position) {
	            return TestFragment.newInstance(CONTENT[position % CONTENT.length]);
	        }

	        @Override
	        public CharSequence getPageTitle(int position) {
	            return CONTENT[position % CONTENT.length].toUpperCase();
	        }

	        @Override public int getIconResId(int index) {
	          return ICONS[index];
	        }

	      @Override
	        public int getCount() {
	          return CONTENT.length;
	        }
	    }
}
