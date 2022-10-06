package com.seafile.seadroid2.loopimages;

import android.app.Service;
import android.appwidget.AppWidgetManager;
import android.appwidget.AppWidgetProvider;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.view.LayoutInflater;
import android.widget.RelativeLayout;
import android.widget.RemoteViews;

import com.seafile.seadroid2.R;

import java.util.List;
import java.util.Map;
import java.util.zip.Inflater;

/**
 * Implementation of App Widget functionality.
 * App Widget Configuration implemented in {@link LoopImagesWidgetConfigureActivity LoopImagesWidgetConfigureActivity}
 */
public class LoopImagesWidget extends AppWidgetProvider {
    public static final String WIDGET_ID_KEY = "widget_id";
    public static final String DIR_INFO = "dir_info";
    public static final String IMAGE_NAME = "image_name";

    static void updateAppWidget(Context context, AppWidgetManager appWidgetManager,
                                int appWidgetId) {

//        CharSequence widgetText = LoopImagesWidgetConfigureActivity.loadTitlePref(context, appWidgetId);
//        // Construct the RemoteViews object
//        List<DirInfo> dirInfos = LoopImagesWidgetConfigureActivity.getDirInfo(appWidgetId);
//
//        RemoteViews views = new RemoteViews(context.getPackageName(), R.layout.loop_images_widget);
//        views.setImageViewResource(R.id.loopimages_imageview, R.drawable.rem);
////
////        // Instruct the widget manager to update the widget
//        appWidgetManager.updateAppWidget(appWidgetId, views);
        Intent intent = new Intent(context, LoopImagesWidgetService.class);
        intent.putExtra(LoopImagesWidgetService.UPDATE_IMAGE_INFO_SIGNAL, appWidgetId);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            context.startForegroundService(intent);
        }else{
            context.startService(intent);
        }
    }

    @Override
    public void onUpdate(Context context, AppWidgetManager appWidgetManager, int[] appWidgetIds) {
        // There may be multiple widgets active, so update all of them
//        for (int appWidgetId : appWidgetIds) {
//            updateAppWidget(context, appWidgetManager, appWidgetId);
//        }
        Intent intent = new Intent(context, LoopImagesWidgetService.class);
        intent.putExtra(LoopImagesWidgetService.UPDATE_IMAGE_INFO_SIGNAL, LoopImagesWidgetService.UPDATE_ALL_WIDGETS);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            context.startForegroundService(intent);
        }else{
            context.startService(intent);
        }
    }

    @Override
    public void onDeleted(Context context, int[] appWidgetIds) {
        // When the user deletes the widget, delete the preference associated with it.
        for (int appWidgetId : appWidgetIds) {
            LoopImagesWidgetConfigureActivity.deleteDirInfo(appWidgetId);
            Intent intent = new Intent(context, LoopImagesWidgetService.class);
            intent.putExtra(LoopImagesWidgetService.DELETE_IMAGE_INFO_SIGNAL, appWidgetId);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                context.startForegroundService(intent);
            }else{
                context.startService(intent);
            }
        }
    }

    @Override
    public void onEnabled(Context context) {
        // Enter relevant functionality for when the first widget is created
    }

    @Override
    public void onDisabled(Context context) {
        // Enter relevant functionality for when the last widget is disabled
    }
}