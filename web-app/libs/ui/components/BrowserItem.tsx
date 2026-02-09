import { MathProblem } from 'web-api-client';
import { useTranslation } from '../provider';
import { Paper } from '../layouts';
import { cn } from '../utils';
import { Button } from './Button';
import { Icon } from './Icon';
import { Label } from './Label';

export interface BrowserItemProps
  extends Pick<MathProblem, 'id' | 'name' | 'description' | 'created' | 'updated'> {
  className?: string;
  onDelete?: (id: number) => void;
  onView?: (id: number) => void;
}

export const BrowserItem: React.FC<BrowserItemProps> = ({
  id,
  name,
  description,
  created,
  updated,
  className,
  onDelete,
  onView,
}) => {
  const t = useTranslation('ui.browserItem');

  return (
    <Paper
      thickness="sm"
      showDivider={false}
      className={cn('border border-white-800 rounded-xl hover:shadow-xl transition', className)}
    >
      <Paper.Title className="flex flex-row gap-2">
        <div className="px-4 py-3 rounded-full bg-primary-base/5">
          <Icon size="xl" name={'pen'} className="text-primary-base"></Icon>
        </div>
        <div className="flex flex-col">
          <Label data-testid={`head-label-${name}`} size="md">
            {name || t('noName')}
          </Label>
          <Label size="sm" className="text-text-grey">
            {description || t('noDescription')}
          </Label>
        </div>
      </Paper.Title>

      <Paper.Content className="flex flex-col gap-2">
        <div className="flex flex-row">
          <Label size="sm" className="text-text-grey m-2">
            <Icon name="clock" className="mr-2"></Icon>
            {t('createdAt')} {created && new Date(created).toLocaleDateString('CS-cs')}
          </Label>
          <Label size="sm" className="text-text-grey m-2">
            <Icon name="clock" className="mr-2"></Icon>
            {t('updatedAt')} {updated && new Date(updated).toLocaleDateString('CS-cs')}
          </Label>
        </div>
        <div className="flex flex-col md:!flex-row gap-2 m-2">
          <Button className="gap-2" onClick={() => onView?.(id!)}>
            <Icon name="eye"></Icon>
            {t('actions.view')}
          </Button>
          <Button variant="error" className="gap-2" onClick={() => onDelete?.(id!)}>
            <Icon name="trash"></Icon> {t('actions.delete')}
          </Button>
        </div>
      </Paper.Content>
    </Paper>
  );
};
