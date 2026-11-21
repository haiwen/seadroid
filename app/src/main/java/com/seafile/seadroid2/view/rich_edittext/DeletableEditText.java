/*
Copyright 2017 yangchong211（github.com/yangchong211）

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package com.seafile.seadroid2.view.rich_edittext;

import android.content.Context;
import android.util.AttributeSet;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputConnection;

import androidx.appcompat.widget.AppCompatEditText;

/**
 * <pre>
 *     @author 杨充
 *     blog  : https://github.com/yangchong211
 *     time  : 2016/3/31
 *     desc  : Custom EditText
 *     revise: Primarily used to invoke OnKeyListener when the soft keyboard's backSpace button is pressed
 * </pre>
 */
public class DeletableEditText extends AppCompatEditText {

    private DeleteInputConnection inputConnection;

    public DeletableEditText(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public DeletableEditText(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public DeletableEditText(Context context) {
        super(context);
        init();
    }


    private void init(){
        inputConnection = new DeleteInputConnection(null,true);
    }

    /**
     * Returns an InputConnection when the input method connects to the EditText.
     * Wrap the InputConnection created by the superclass method and return our own wrapper.
     * @param outAttrs                          attrs
     * @return
     */
    @Override
    public InputConnection onCreateInputConnection(EditorInfo outAttrs) {
        //inputConnection = new DeleteInputConnection(super.onCreateInputConnection(outAttrs), true);
        inputConnection.setTarget(super.onCreateInputConnection(outAttrs));
        return inputConnection;
    }

    /**
     * Sets a backspace listener, mainly to handle deletion of empty content with Sogou input on some phones
     * @param listener                          listener
     */
    public void setBackSpaceListener(DeleteInputConnection.BackspaceListener listener){
        inputConnection.setBackspaceListener(listener);
    }


}
