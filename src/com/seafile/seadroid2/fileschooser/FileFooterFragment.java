package com.seafile.seadroid2.fileschooser;


import com.seafile.seadroid2.R;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;

public class FileFooterFragment extends Fragment {

    private TextView statusView;
    private Button cancelButton;
    private Button confirmButton;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {
        View root = inflater.inflate(R.layout.file_list_footer, container, false);
        statusView = (TextView)root.findViewById(R.id.upload_selection_status);
        cancelButton = (Button)root.findViewById(R.id.button_cancel_upload);
        confirmButton = (Button)root.findViewById(R.id.button_confirm_upload);
        confirmButton.setEnabled(false);

        cancelButton.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                ((MultiFileChooserActivity)getActivity()).onCancelButtonClicked();

            }
        });

        confirmButton.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                ((MultiFileChooserActivity)getActivity()).onConfirmButtonClicked();

            }
        });

        return root;
    }

    public TextView getStatusView() {
        return statusView;
    }

    public Button getConfirmButton() {
        return confirmButton;
    }

    @Override
    public void onPause() {
        super.onPause();
    }
}
