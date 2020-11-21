package com.osallek.eu4saveeditor.controller.object;

import com.osallek.eu4parser.model.game.IdeaGroup;

import java.util.Map;

public class Idea {

    private IdeaGroup ideaGroup;

    private int level;

    private boolean changed;

    public Idea(IdeaGroup ideaGroup) {
        this.ideaGroup = ideaGroup;
        this.level = 0;
    }

    public Idea(IdeaGroup ideaGroup, Integer amount) {
        this.ideaGroup = ideaGroup;
        this.level = amount;
        this.changed = true;
    }

    public Idea(Idea other) {
        this.ideaGroup = other.ideaGroup;
        this.level = other.level;
        this.changed = other.changed;
    }

    public Idea(Map.Entry<IdeaGroup, Integer> entry) {
        this.ideaGroup = entry.getKey();
        this.level = entry.getValue();
    }

    public IdeaGroup getIdeaGroup() {
        return ideaGroup;
    }

    public void setIdeaGroup(IdeaGroup ideaGroup) {
        if (!ideaGroup.equals(this.ideaGroup)) {
            this.ideaGroup = ideaGroup;
            this.changed = true;
        }
    }

    public int getLevel() {
        return level;
    }

    public void setLevel(int level) {
        if (level != this.level) {
            this.level = level;
            this.changed = true;
        }
    }

    public boolean isChanged() {
        return changed;
    }

    @Override
    public String toString() {
        return this.ideaGroup.getLocalizedName();
    }
}
