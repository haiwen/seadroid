package com.seafile.seadroid2.framework;

import static org.junit.Assert.assertEquals;
import android.app.Application;
import android.view.View;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import com.seafile.seadroid2.enums.FileViewType;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.ui.repo.RepoGridLayoutManager;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.annotation.Config;

@RunWith(RobolectricTestRunner.class)
@Config(sdk = 28, manifest = Config.NONE, application = Application.class)
public class RepoGridLayoutManagerTest {
    @Test public void returningFromGalleryRestoresFullWidthLibraries() {
        verifyRoundTrip(FileViewType.GALLERY, 200);
    }

    @Test public void returningFromGridRestoresFullWidthLibraries() {
        verifyRoundTrip(FileViewType.GRID, 400);
    }

    private void verifyRoundTrip(FileViewType type, int fileWidth) {
        RecyclerView recycler = new RecyclerView(RuntimeEnvironment.getApplication());
        List<BaseModel> items = new ArrayList<>();
        RecyclerView.Adapter<RecyclerView.ViewHolder> adapter = new RecyclerView.Adapter<>() {
            @NonNull @Override public RecyclerView.ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
                View view = new View(parent.getContext());
                view.setLayoutParams(new RecyclerView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, 80));
                return new RecyclerView.ViewHolder(view) {};
            }
            @Override public void onBindViewHolder(@NonNull RecyclerView.ViewHolder holder, int position) {}
            @Override public int getItemCount() { return items.size(); }
        };
        recycler.setAdapter(adapter);
        for (int repeat = 0; repeat < 3; repeat++) {
            recycler.setLayoutManager(new RepoGridLayoutManager(recycler.getContext(), false, type, () -> items));
            items.clear();
            items.add(new RepoModel());
            items.add(new RepoModel());
            adapter.notifyDataSetChanged();
            measure(recycler);
            assertEquals(800, recycler.getChildAt(0).getWidth());
            assertEquals(80, recycler.getChildAt(0).getHeight());

            recycler.setLayoutManager(new RepoGridLayoutManager(recycler.getContext(), true, type, () -> items));
            items.clear();
            for (int i = 0; i < 4; i++) items.add(new DirentModel());
            adapter.notifyDataSetChanged();
            measure(recycler);
            assertEquals(fileWidth, recycler.getChildAt(0).getWidth());
        }
        recycler.setLayoutManager(new RepoGridLayoutManager(recycler.getContext(), false, type, () -> items));
        items.clear();
        items.add(new RepoModel());
        adapter.notifyDataSetChanged();
        measure(recycler);
        assertEquals(800, recycler.getChildAt(0).getWidth());
    }

    private void measure(RecyclerView recycler) {
        recycler.measure(View.MeasureSpec.makeMeasureSpec(800, View.MeasureSpec.EXACTLY),
                View.MeasureSpec.makeMeasureSpec(1200, View.MeasureSpec.EXACTLY));
        recycler.layout(0, 0, 800, 1200);
    }
}
