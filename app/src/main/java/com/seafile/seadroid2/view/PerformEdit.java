/*
 * Copyright 2016. SHENQINCI(沈钦赐)<946736079@qq.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.seafile.seadroid2.view;

import android.text.Editable;
import android.text.TextWatcher;
import android.widget.EditText;

import androidx.annotation.NonNull;

import java.util.Stack;

/**
 * Undo and redo
 * Created by 沈钦赐 on 16/6/23.
 */
public class PerformEdit {
    //Operation index (one edit may correspond to multiple operations, e.g. replacing text is delete + insert)
    int index;
    //Undo stack
    Stack<Action> history = new Stack<>();
    //Redo stack
    Stack<Action> historyBack = new Stack<>();

    private Editable editable;
    private EditText editText;
    //Automatic operation flag to prevent repeated callbacks and infinite undo
    private boolean flag = false;

    public PerformEdit(@NonNull EditText editText) {
        CheckNull(editText, "EditText cannot be null");
        this.editable = editText.getText();
        this.editText = editText;
        editText.addTextChangedListener(new Watcher());
    }

    protected void onEditableChanged(Editable s) {

    }

    protected void onTextChanged(Editable s) {

    }


    /**
     * Clear history.
     */
    public final void clearHistory() {
        history.clear();
        historyBack.clear();
    }


    /**
     * Undo.
     */
    public final void undo() {
        if (history.empty()) return;
        //Lock operation
        flag = true;
        Action action = history.pop();
        historyBack.push(action);
        if (action.isAdd) {
            //Undo add
            editable.delete(action.startCursor, action.startCursor + action.actionTarget.length());
            editText.setSelection(action.startCursor, action.startCursor);
        } else {
            //Undo delete
            editable.insert(action.startCursor, action.actionTarget);
            if (action.endCursor == action.startCursor) {
                editText.setSelection(action.startCursor + action.actionTarget.length());
            } else {
                editText.setSelection(action.startCursor, action.endCursor);
            }
        }
        //Release operation
        flag = false;
        //Check whether the next action is the same operation as this one, until they differ
        if (!history.empty() && history.peek().index == action.index) {
            undo();
        }
    }

    /**
     * Redo.
     */
    public final void redo() {
        if (historyBack.empty()) return;
        flag = true;
        Action action = historyBack.pop();
        history.push(action);
        if (action.isAdd) {
            //Redo add
            editable.insert(action.startCursor, action.actionTarget);
            if (action.endCursor == action.startCursor) {
                editText.setSelection(action.startCursor + action.actionTarget.length());
            } else {
                editText.setSelection(action.startCursor, action.endCursor);
            }
        } else {
            //Redo delete
            editable.delete(action.startCursor, action.startCursor + action.actionTarget.length());
            editText.setSelection(action.startCursor, action.startCursor);
        }
        flag = false;
        //Check whether the next action is the same operation as this one
        if (!historyBack.empty() && historyBack.peek().index == action.index)
            redo();
    }

    /**
     * Set default text.
     */
    public final void setDefaultText(CharSequence text) {
        clearHistory();
        flag = true;
        editable.replace(0, editable.length(), text);
        flag = false;
    }

    private class Watcher implements TextWatcher {

        /**
         * Before text changed.
         *
         * @param s     the s
         * @param start the start cursor
         * @param count the endCursor selection count
         * @param after the after number of characters added by the replacement
         */
        @Override
        public final void beforeTextChanged(CharSequence s, int start, int count, int after) {
            if (flag) return;
            int end = start + count;
            if (end > start && end <= s.length()) {
                CharSequence charSequence = s.subSequence(start, end);
                //Text deleted
                if (charSequence.length() > 0) {
                    Action action = new Action(charSequence, start, false);
                    if (count > 1) {
                        //If more than one character at a time, the user selected text and then replaced or deleted it
                        action.setSelectCount(count);
                    } else if (count == 1 && count == after) {
                        //Single character replacement
                        action.setSelectCount(count);
                    }
                    //Another case: select a character and then delete it (not considered for now)

                    history.push(action);
                    historyBack.clear();
                    action.setIndex(++index);
                }
            }
        }

        /**
         * On text changed.
         *
         * @param s      the s
         * @param start  the start cursor
         * @param before the before selection count
         * @param count  the endCursor number added
         */
        @Override
        public final void onTextChanged(CharSequence s, int start, int before, int count) {
            if (flag) return;
            int end = start + count;
            if (end > start) {
                CharSequence charSequence = s.subSequence(start, end);
                //Text added
                if (charSequence.length() > 0) {
                    Action action = new Action(charSequence, start, true);

                    history.push(action);
                    historyBack.clear();
                    if (before > 0) {
                        //Text replacement (delete first, then add); delete and add are the same operation, so no need to increment the index
                        action.setIndex(index);
                    } else {
                        action.setIndex(++index);
                    }
                }
            }
        }

        @Override
        public final void afterTextChanged(Editable s) {
            if (flag) return;
            if (s != editable) {
                editable = s;
                onEditableChanged(s);
            }
            PerformEdit.this.onTextChanged(s);
        }

    }

    private class Action {
        /**
         * Changed characters.
         */
        CharSequence actionTarget;
        /**
         * Cursor position.
         */
        int startCursor;
        int endCursor;
        /**
         * Flag for add operation.
         */
        boolean isAdd;
        /**
         * Operation index.
         */
        int index;


        public Action(CharSequence actionTag, int startCursor, boolean add) {
            this.actionTarget = actionTag;
            this.startCursor = startCursor;
            this.endCursor = startCursor;
            this.isAdd = add;
        }

        public void setSelectCount(int count) {
            this.endCursor = endCursor + count;
        }

        public void setIndex(int index) {
            this.index = index;
        }
    }


    private static void CheckNull(Object o, String message) {
        if (o == null) throw new IllegalStateException(message);
    }
}
